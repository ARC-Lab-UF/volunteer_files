-- Greg Stitt
-- University of Florida

-- Description:
-- fifo_vw (variable write)


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generic Descriptions
-- data_width : The width of a single element stored in the FIFO
-- parallel_io : The maximum number of parallel inputs written simulateously,
--               and the exact number of outputs provided by the FIFO.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions
-- clk: clock
-- rst: reset
-- rd : read data from the buffer (does nothing when empty is asserted)
-- wr       : writes up to parallel_io inputs into the buffer, based on status
--            of valid_in (does nothing when full is asserted)
-- valid_in : specifies valid status of all parallel_io inputs (1 bit for each).
--            The fifo will ignore any inputs whose valid_in bit is
--            not asserted. NOTE: the FIFO does not allow invalid inputs
--            with an index higher than valid inputs. e.g. for
--            parallel_io = 4, "1100" would be legal, but "0101" would be
--            illegal. 
-- empty   : asserted when the buffer is empty (has less than parallel_io
--           elements in fifo)
-- full    : asserted when there isn't room to write parallel_io inputs.
-- input   : parallel_io input values, concatenated into a big std_logic_vector
-- output  : max_outputs outputs. When rd_amount < max_outputs, the higher bits
--            contains valid datas.
-------------------------------------------------------------------------------

entity fifo_vw is
    generic (
        data_width  : positive := 16;
        parallel_io : positive := 16);
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        rd          : in  std_logic;
        wr          : in  std_logic;
        --valid_in : in  std_logic_vector(parallel_io-1 downto 0);
        valid_count :     std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
        empty       : out std_logic;
        full        : out std_logic;
        input       : in  std_logic_vector(parallel_io*data_width-1 downto 0);
        output      : out std_logic_vector(parallel_io*data_width-1 downto 0));
end fifo_vw;

--architecture old of fifo_vw is

--    type data_array is array (natural range <>) of std_logic_vector(data_width-1 downto 0);

--    -- -1 is hack for simulation error, should be -2
--    signal front          : integer range 0 to parallel_io*2-1; 
--    signal valid_in_count : integer range 0 to parallel_io;

--    signal inputs : data_array(0 to parallel_io-1);
--    signal regs   : data_array(0 to parallel_io*2-1);

--    signal valid_wr : std_logic;
--    signal valid_rd : std_logic;
--    signal full_s   : std_logic;
--    signal empty_s  : std_logic;

--begin

    
    
--    -- empty when there less than parallel_io valid elements stored
--    empty_s <= '1' when front < parallel_io else '0';
--    empty   <= empty_s;

--    -- full when there are >= parallel_io elements, but not when a read is
--    -- currently occuring
--    full_s <= '1' when front >= parallel_io and valid_rd = '0' else '0';
--    full   <= full_s;

--    -- check for valid rd/wr to avoid data loss
--    valid_wr <= wr and not full_s;
--    valid_rd <= rd and not empty_s;

--    -- devectorize the input vector into an array
--    -- not necessary, but makes other code more concise
--    process(input)
--    begin
--        for i in 0 to parallel_io-1 loop
--            inputs(i) <= input(input'length-i*data_width-1 downto input'length-(i+1)*data_width);
--        end loop;
--    end process;

--    -- calculate the number of valid inputs
--    process(valid_in)
--        variable temp : integer range 0 to parallel_io;
--    begin
--        temp := 0;
--        for i in parallel_io-1 downto 0 loop
--            exit when valid_in(i) = '0'; 
----            if (valid_in(i) = '1') then
--            temp := temp + 1;
--  --          end if;
--        end loop;

--        valid_in_count <= temp;
--    end process;

--    -- vectorize the parallel_io registers into the output signal
--    process(regs)
--    begin
--        for i in 0 to parallel_io-1 loop
--            output(input'length-i*data_width-1 downto input'length-(i+1)*data_width) <= regs(i);
--        end loop;
--    end process;

--    process(clk, rst)
--        -- -1 is hack for simulation error, should be -2
--        variable new_front : integer range 0 to parallel_io*2-1;
--    begin
--        if (rst = '1') then

--            for i in 0 to parallel_io*2-1 loop
--                regs(i) <= (others => '0');
--            end loop;

--            front <= 0;

--        elsif (rising_edge(clk)) then

--            new_front := front;

--            if (valid_rd = '1') then
--                -- every read should move the front index by parallel_io
--                new_front := new_front - parallel_io;

--                -- move the top half of the buffer to the bottom
--                for i in 0 to parallel_io-1 loop
--                    regs(i) <= regs(i+parallel_io);
--                end loop;
--            end if;

--            -- write new data to the parallel_io registers starting at new_front
--            if (valid_wr = '1') then
--                for i in 0 to parallel_io-1 loop
--                    regs(new_front+i) <= inputs(i);
--                end loop;

--                new_front := new_front + valid_in_count;
--            end if;

--            front <= new_front;
--        end if;
--    end process;

--end OLD;

architecture HIGH_FREQ of fifo_vw is

    constant LEVELS : positive := bitsNeeded(PARALLEL_IO-1);

    type count_array is array (integer range <>) of unsigned(bitsNeeded(PARALLEL_IO)-1 downto 0);

    signal buf          : std_logic_vector(input'length*2-1 downto 0);
    signal buf_count    : unsigned(bitsNeeded(2*parallel_io)-1 downto 0);
    signal count        : count_array(0 to LEVELS-1);
    signal offset       : count_array(0 to LEVELS-1);
    signal shift_amount : std_logic_vector(bitsNeeded(parallel_io-1)-1 downto 0);
    signal shift_done   : std_logic;

    signal full_s         : std_logic;
    signal empty_s        : std_logic;
    signal valid_wr       : std_logic;
    signal valid_rd       : std_logic;
    signal input_extended : std_logic_vector(input'length*2-1 downto 0);
    signal input_aligned  : std_logic_vector(input'length*2-1 downto 0);
    signal shift_en       : std_logic;

    signal input_fifo_rd : std_logic;
    signal input_fifo_empty : std_logic;        
    signal input_fifo_full : std_logic;
    signal input_fifo_data_out : std_logic_vector(input'range);
    signal count_fifo_data_out : std_logic_vector(valid_count'range);

    constant FIFO_DEPTH : positive := 4;
    
begin
    
    -- FIFOs to decouple reads and writes, used to improve timing
    U_INPUT_FIFO : entity work.fifo
        generic map (
            width             => input'length,
            depth             => FIFO_DEPTH,
            same_cycle_output => true)
        port map (
            clk         => clk,
            rst         => rst,
            rd          => input_fifo_rd,
            wr          => valid_wr,
            empty       => input_fifo_empty,
            full        => input_fifo_full,
            almost_full => open,
            input       => input,
            output      => input_fifo_data_out);

    input_fifo_rd <= not full_s and not input_fifo_empty;
    
    U_COUNT_FIFO : entity work.fifo
        generic map (
            width             => valid_count'length,
            depth             => FIFO_DEPTH,
            same_cycle_output => true)
        port map (
            clk         => clk,
            rst         => rst,
            rd          => input_fifo_rd,
            wr          => valid_wr,
            empty       => open,
            full        => open,
            almost_full => open,
            input       => valid_count,
            output      => count_fifo_data_out);
    
    input_extended <= input_fifo_data_out & std_logic_vector(to_unsigned(0, input'length));

    U_SHIFT_INPUT : entity work.right_shift
        generic map (
            shift_bits => LEVELS,
            word_width => data_width,
            num_words  => 2*PARALLEL_IO)
        port map (
            clk          => clk,
            rst          => rst,
            en           => shift_en,
            input        => input_extended,
            output       => input_aligned,
            shift_amount => shift_amount,
            valid_in     => input_fifo_rd,
            valid_out    => shift_done);

    shift_en <= not full_s or valid_rd;

    full_s  <= '1' when buf_count >= parallel_io and valid_rd = '0' else '0';
    empty_s <= '1' when buf_count < parallel_io  else '0';

    full  <= input_fifo_full;
    empty <= empty_s;

    valid_wr <= '1' when wr = '1' and input_fifo_full = '0' and unsigned(valid_count) > 0 else '0';
    valid_rd <= rd and not empty_s;

    shift_amount <= std_logic_vector(offset(0)(shift_amount'range));

    process(clk, rst)
        variable next_buf             : std_logic_vector(buf'range);
        variable next_buf_count       : unsigned(buf_count'range);
    begin
        if (rst = '1') then
            buf       <= (others => '0');
            buf_count <= (others => '0');

            for i in 0 to bitsNeeded(parallel_io-1)-1 loop
                count(i)  <= (others => '0');
                offset(i) <= (others => '0');
            end loop;
            
        elsif (rising_edge(clk)) then

            -- shift the counts of each level
            if (shift_en = '1') then
                count(0) <= (others => '0');
                for i in 0 to bitsNeeded(parallel_io-1)-2 loop
                    count(i+1)  <= count(i);
                    offset(i+1) <= offset(i);
                end loop;
            end if;
            
            -- update the count at the first level
            if (input_fifo_rd = '1') then
                count(0) <= unsigned(count_fifo_data_out);

                -- make sure the count is between 0 and parallel_io-1
                if (offset(0) + unsigned(count_fifo_data_out) >= parallel_io) then
                    offset(0) <= offset(0) + unsigned(count_fifo_data_out) - parallel_io;
                else
                    offset(0) <= offset(0) + unsigned(count_fifo_data_out);
                end if;
--            else
--                count(0) <= (others => '0');
            end if;

            -- shift the counts of each level
            if (shift_en = '1') then
                for i in 0 to bitsNeeded(parallel_io-1)-2 loop
                    count(i+1)  <= count(i);
                    offset(i+1) <= offset(i);
                end loop;
            end if;

            next_buf_count := buf_count;
            next_buf       := buf;

            -- update the count of the internal buffer
            if (valid_rd = '1') then
                -- get the count of the buffer after the read but before
                -- writing anything new
                next_buf_count := next_buf_count - parallel_io;

                -- shift by parallel_io on a read to align extra data
                next_buf := buf(buf'length/2-1 downto 0) & std_logic_vector(to_unsigned(0, buf'length/2));
            end if;

            -- if the shifter has a valid output and isn't stalled
            -- then update the internal buffer
            if (shift_done = '1' and shift_en = '1') then
                -- update all the invalid elements in the buffer
                for i in next_buf'range loop
                    if ((next_buf'length-(i+1)) / data_width >= next_buf_count) then
                        next_buf(i) := input_aligned(i);
                    end if;
                end loop;

                next_buf_count := next_buf_count + count(LEVELS-1);
            end if;

            buf_count <= next_buf_count;
            buf       <= next_buf;
        end if;
    end process;

    -- the output is always the top half of the buffer
    output <= buf(buf'length-1 downto buf'length/2);
    
end HIGH_FREQ;
