--  Copyright (c) University of Florida
--
--  This file is part of window_gen.
--
--  window_gen is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  window_gen is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with window_gen.  If not, see <http://www.gnu.org/licenses/>.

-- Greg Stitt
-- University of Florida

-- Description:
-- fifo_vr (variable read)
-- This entity implements a fifo that writes a fixed (but configurable)
-- number of inputs in parallel, while allowing a variable number of outputs
-- to be read each cycle.
--
-- The entity is useful for streams of data where upstream components always
-- produce a fixed amount of data, but downstream components may need to
-- read a variable amount of data. e.g., when streaming an image sequentially
-- from memory into an FPGA, the memory might provide four pixels per cycle.
-- If the circuit buffers each row into separate on-chip memories, then at the
-- end of a row, the circuit may need to read less than four pixels if the # of
-- columns is not a multiple of four. In this case, the buffer enables the
-- memory to continually write four pixels per cycle, while the downstream
-- circuit reads as much as needed.
--
-- The size of the fifo is fixed because its main purpose is to dynamically
-- read varying amounts of data from a stream. If a typical FIFO is needed for
-- buffering, the user should connect that FIFO to the input of this
-- entity.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generic Descriptions
-- data_width : The width of a single element to read from the FIFO
-- parallel_io : The number of parallel inputs written every cycle, which
--               is also the max number of outputs that can be read each
--               cycle.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: clock
-- rst: asynchronous reset
-- rd : read data from the buffer (does nothing when empty is asserted)
-- rd_amount : The amount of elements to read from the buffer when rd is
--             asserted. Note that if rd_amount is larger than the actual
--             amount of elements in the fifo, the fifo returns only what
--             it has stored. e.g., If the fifo has 2 elements and rd_amount
--             is 3 when rd is asserted, the fifo will output two valid
--             elements and a third junk element.
-- wr       : write num_inputs inputs into the buffer (does nothing when
--            full is asserted)
-- empty   : asserted when the buffer is empty (has 0 elements in fifo)
-- full    : asserted when there isn't room to write num_inputs inputs.
-- input   : num_inputs input values, concatenated into a big std_logic_vector
-- output  : max_outputs outputs. When rd_amount < max_outputs, the higher bits
--            contains valid datas.
-- count     : the number of valid data_width elements in the fifo
-- valid_out : individual valid bits for each output
-------------------------------------------------------------------------------

entity fifo_vr is
    generic (
        data_width     : positive;
        parallel_io    : positive;
        input0_at_MSB  : boolean := false;
        output0_at_MSB : boolean := false);
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        rd        : in  std_logic;
        rd_amount : in  std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
        wr        : in  std_logic;
        stall     : in  std_logic;
        empty     : out std_logic;
        full      : out std_logic;
        input     : in  std_logic_vector(parallel_io*data_width-1 downto 0);
        output    : out std_logic_vector(parallel_io*data_width-1 downto 0);
        count     : out std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
        valid_out : out std_logic_vector(parallel_io-1 downto 0));
end fifo_vr;


architecture BARREL_SHIFTER of fifo_vr is

    type data_array is array (natural range <>) of std_logic_vector(data_width-1 downto 0);
    signal count_s       : unsigned(bitsNeeded(parallel_io*2)-1 downto 0);
    signal count_r       : unsigned(bitsNeeded(parallel_io*2)-1 downto 0);
    signal front         : unsigned(bitsNeeded(parallel_io-1)-1 downto 0);
    signal regs          : data_array(0 to parallel_io*2-1);
    signal valid_wr      : std_logic;
    signal valid_rd      : std_logic;
    signal full_s        : std_logic;
    signal empty_s       : std_logic;
    signal valid         : std_logic_vector(0 to parallel_io*2-1);
    signal valid_shifted : std_logic_vector(0 to parallel_io*2-1);
    signal valid_aligned : std_logic_vector(0 to parallel_io-1);
    signal valid_out_s   : std_logic_vector(0 to parallel_io-1);
    signal inputs        : data_array(0 to parallel_io-1);
    signal window_reset  : std_logic;
    --signal rd_amount_s   : integer range 0 to parallel_io;
    signal rd_amount_s   : unsigned(bitsNeeded(parallel_io)-1 downto 0);

    signal output_unaligned : std_logic_vector(PARALLEL_IO*2*data_width-1 downto 0);
    signal output_aligned   : std_logic_vector(PARALLEL_IO*2*data_width-1 downto 0);
    signal output_valid     : std_logic;
    signal en               : std_logic;

    signal fifo_empty   : std_logic;
    signal fifo_full    : std_logic;
    signal fifo_rd_en   : std_logic;
    signal fifo_rd_data : std_logic_vector(input'range);
    
begin
    -- buffers input to decouple reads and writes
    -- previously, full depended on rd_en, which caused timing problems
    U_INPUT_FIFO : entity work.fifo
        generic map (
            width             => input'length,
            depth             => 4,  -- can probably be shrunk to 2 buts needs testing
            same_cycle_output => true)
        port map (
            clk         => clk,
            rst         => rst,
            rd          => fifo_rd_en,
            wr          => valid_wr,
            empty       => fifo_empty,
            full        => fifo_full,
            almost_full => open,
            input       => input,
            output      => fifo_rd_data);  

    fifo_rd_en <= not fifo_empty and not full_s;

    en      <= not stall;
    empty_s <= not valid(0) or stall;
    --empty_s <= not valid(0);
    empty   <= empty_s;

    -- the buffer is full when any of the valid bits in the upper half are
    -- asserted, but not when there is a valid read that resets the window
    full_s <= (valid(parallel_io) and not (valid_rd and window_reset) and
               not empty_s) or stall;
    --full <= full_s;
    full <= fifo_full;

    -- check for valid rd/wr to avoid data loss
    --valid_wr <= wr and not full_s;
    valid_wr <= wr and not fifo_full;
    valid_rd <= '1' when rd = '1' and empty_s = '0' and unsigned(rd_amount) > 0 else '0';

    count     <= (others => '0') when empty_s = '1' else std_logic_vector(count_r(count'range)) when count_r <= parallel_io else std_logic_vector(to_unsigned(parallel_io, count'length));
    valid_out <= valid_out_s;

    -- the window position is reset any time front extends past the first half
    window_reset <= '0' when front + rd_amount_s < parallel_io else '1';

    -- devectorize the input vector into an array based on the whether or not
    -- the first input is at the MSB or LDB
    U_INPUT0_AT_MSB : if (INPUT0_AT_MSB) generate
        process(fifo_rd_data)
        begin
            for i in 0 to parallel_io-1 loop
                inputs(i) <= fifo_rd_data(input'length-i*data_width-1 downto input'length-(i+1)*data_width);
            end loop;
        end process;
    end generate;

    U_INPUT0_AT_LSB : if (INPUT0_AT_MSB = false) generate
        process(fifo_rd_data)
        begin
            for i in 0 to parallel_io-1 loop
                inputs(i) <= fifo_rd_data((i+1)*data_width-1 downto i*data_width);
            end loop;
        end process;
    end generate;

    -- update the count
    process(count_r, rd_amount_s, valid_rd, fifo_rd_en)
    begin
        count_s <= count_r;

--        if (valid_wr = '1') then
        if (fifo_rd_en = '1') then
            if (valid_rd = '1') then
                count_s <= count_r + parallel_io - rd_amount_s;
            else
                count_s <= count_r + parallel_io;
            end if;
        end if;

--        if (valid_rd = '1' and valid_wr = '0') then
        if (valid_rd = '1' and fifo_rd_en = '0') then
            count_s <= count_r - rd_amount_s;
        end if;
    end process;

    -- make sure rd_amount can't exceed number of valid outputs
    process(rd_amount, count_r)
    begin
        if (unsigned(rd_amount) > count_r) then
            if (count_r > parallel_io) then
                rd_amount_s <= to_unsigned(parallel_io, rd_amount_s'length);
            else
                rd_amount_s <= count_r(rd_amount'range);
            end if;
        else
            rd_amount_s <= unsigned(rd_amount);
        end if;
    end process;

    -- vectorize the registers for input to the shifter
    process(regs)
    begin
        for i in 0 to PARALLEL_IO*2-1 loop
            output_unaligned(output_unaligned'length-(i*data_width)-1 downto output_unaligned'length-(i+1)*data_width) <= regs(i);
        end loop;
    end process;

    -- realign the window so the first element is on the left
    U_LEFT_SHIFT_DATA : entity work.left_shift
        generic map (
            shift_bits => bitsNeeded(PARALLEL_IO-1),
            word_width => data_width,
            num_words  => 2*PARALLEL_IO)
        port map (
            clk          => clk,
            rst          => rst,
            en           => en,
            input        => output_unaligned,
            output       => output_aligned,
            shift_amount => std_logic_vector(front),
            valid_in     => valid_rd,
            valid_out    => output_valid);

    U_LEFT_SHIFT_VALID : entity work.left_shift
        generic map (
            shift_bits => bitsNeeded(PARALLEL_IO-1),
            word_width => 1,
            num_words  => 2*PARALLEL_IO)
        port map (
            clk          => clk,
            rst          => rst,
            en           => en,
            input        => valid,
            output       => valid_shifted,
            shift_amount => std_logic_vector(front),
            valid_in     => valid_rd,
            valid_out    => open);

    -- actual valid bits are in the top half of the shifter output
    valid_aligned <= valid_shifted(0 to PARALLEL_IO-1);

    -- reorder the outputs based on the OUTPUT0_AT_MSB setting
    process(output_aligned, output_valid, valid_aligned)
    begin
        if (OUTPUT0_AT_MSB) then
            output <= output_aligned(output_aligned'length-1 downto output_aligned'length/2);
            for i in 0 to PARALLEL_IO-1 loop
                --valid_out_s(i) <= valid_aligned(PARALLEL_IO-i-1) and output_valid;
                valid_out_s(i) <= valid_aligned(i) and output_valid;
            end loop;
        else
            for i in 0 to PARALLEL_IO-1 loop
                -- reverse the order of the words in the outputs
                output((i+1)*data_width-1 downto i*data_width) <= output_aligned(output_aligned'length-i*data_width-1 downto output_aligned'length-(i+1)*data_width);
                valid_out_s(parallel_io-i-1)                   <= valid_aligned(i) and output_valid;
            end loop;
        end if;
    end process;

    -- track the front of the window
    process(clk, rst)
        variable new_front : unsigned(rd_amount_s'range);
    begin
        if (rst = '1') then
            for i in 0 to parallel_io*2-1 loop
                regs(i)  <= (others => '0');
                valid(i) <= '0';
            end loop;

            front   <= (others => '0');
            count_r <= (others => '0');

        elsif (rising_edge(clk)) then

            count_r <= count_s;

            -- during a read, slide the front of the window so the next output
            -- is aligned properly. 
            if (valid_rd = '1') then
                if (window_reset = '0') then
                    new_front := front + rd_amount_s;
                    front     <= new_front(front'range);
                --front <= front + rd_amount_s;
                --assert(new_front(new_front'length-1) = '0') report integer'image(to_integer(new_front)) severity failure;
                else
                    new_front := front + rd_amount_s - parallel_io;
                    front     <= new_front(front'range);
                --assert(new_front(new_front'length-1) = '0') severity failure;
                end if;
            end if;

            -- check if the bottom half is empty. All parallel_io valid bits
            -- of each half should be the same, so only the first bit has to be
            -- checked
            if (valid(0) = '0' or
                (valid_rd = '1' and window_reset = '1')) then

                -- move the top half to the bottom.
                for i in 0 to parallel_io-1 loop
                    regs(i)  <= regs(i+parallel_io);
                    valid(i) <= valid(i+parallel_io);
                end loop;

                -- reset the valid bits for the top half
                for i in parallel_io to parallel_io*2-1 loop
                    valid(i) <= '0';
                end loop;
            end if;

            -- write new data to the top half of buffer
            --if (valid_wr = '1') then
            if (fifo_rd_en = '1') then
                for i in 0 to parallel_io-1 loop
                    regs(i+parallel_io)  <= inputs(i);
                    valid(i+parallel_io) <= '1';
                end loop;
            end if;

        end if;
    end process;

end BARREL_SHIFTER;
