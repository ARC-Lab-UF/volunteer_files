-- Greg Stitt
-- University of Florida

-- Description:
-- This file implements the fifo_core entity, which defines the architecture the
-- fifo entity (see fifo.vhd). 
--
-- The entity contains architectures for using flips (FF) or memory (MEMORY)
-- when synthesized.

-- Notes:
-- The fifo protects against invalid writes (i.e. when full) and invalid reads
-- (i.e. when empty)
--
-- use_bram = true and same_cycle_output = true is not supported by
-- all FPGAs.
--
-- All FIFO depths are currently rounded up to the nearest power of two.

-- Used entities:
-- ram (in BRAM architecture)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generics Description
-- width             : the width of the FIFO in bits (required)
-- depth             : the depth of the FIFO in words (required)
-- almost_full_space : the amount of space left in the FIFO when almost_full
--                     is asserted (required)
-- use_bram          : uses bram when true, uses LUTs/FFs when false (required)
-- same_cycle_output : output appears in same cycle as read when true, one
--                     cycle later when false (required)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Description:
-- clk    : clock input
-- rst    : reset input (asynchronous)
-- rd     : read input (active high)
-- wr     : write input (active high)
-- empty  : empty output (active high)
-- full   : full output (active high)
-- almost_full : asserted when there is almost_full_space left (active high)
-- input  : fifo input
-- output : fifo output
-------------------------------------------------------------------------------

entity fifo_core is
    generic(width             : positive;
            depth             : positive;
            almost_full_space : natural;
            use_bram          : boolean;
            same_cycle_output : boolean);
    port(clk         : in  std_logic;
         rst         : in  std_logic;
         rd          : in  std_logic;
         wr          : in  std_logic;
         empty       : out std_logic;
         full        : out std_logic;
         almost_full : out std_logic;
         input       : in  std_logic_vector(width-1 downto 0);
         output      : out std_logic_vector(width-1 downto 0));
end fifo_core;


-- FF architecture
-- This architecture implements the FIFO with flip-flops

architecture FF of fifo_core is

    type reg_array is array (0 to depth-1) of std_logic_vector(width-1 downto 0);
    signal regs              : reg_array;
    signal count, next_count : unsigned(bitsNeeded(depth)-1 downto 0);
    signal valid_wr          : std_logic;
    signal valid_rd          : std_logic;
    signal empty_s           : std_logic;
    signal full_s            : std_logic;
    
begin

    -- create a shift register to act as the fifo
    -- always writes to register 0
    process(clk, rst)
    begin
        if (rst = '1') then
            -- initialize all registers to 0 (unnessary, useful for debugging)
            for i in 0 to depth-1 loop
                regs(i) <= (others => '0');
            end loop;
        elsif (rising_edge(clk)) then
            -- shift in input everytime there is a valid write
            if (valid_wr = '1') then
                regs(0) <= input;
                for i in 0 to depth-2 loop
                    regs(i+1) <= regs(i);
                end loop;
            end if;
        end if;
    end process;

    -- assign the 1-cycle delayed output if applicable
    U_OUTPUT_NEXT_CYCLE : if same_cycle_output = false generate
        process(clk, rst)
        begin
            if (rst = '1') then
                output <= (others => '0');
            elsif(rising_edge(clk)) then
                -- count-1 is the front of the fifo
                if (count = 0) then
                    -- special case when fifo is empty. One alternative is to
                    -- increase the fifo depth to be a power of two,
                    -- in which case this isn't necessary.
                    output <= regs(0);
                else
                    output <= regs(to_integer(count-1));
                end if;
            end if;
        end process;
    end generate;

    -- assign the output in the same cycle if applicable
    U_OUTPUT_SAME_CYCLE : if same_cycle_output = true generate
        process(regs, count)
        begin
            -- count-1 is the front of the fifo
            if (count = 0) then
                -- special case when fifo is empty. One alternative is to
                -- require the fifo depth to be a power of two,
                -- in which case this isn't necessary.                
                output <= regs(0);
            else
                output <= regs(to_integer(count-1));
            end if;
        end process;
    end generate;

    -- update empty flag
    empty_s <= '1' when count = 0 else '0';
    empty   <= empty_s;

    -- update full flag
    --full_s <= '0' when rd = '1' else
    --          '1' when count = depth else '0';
    full_s <= '1' when count = depth else '0';
    full <= full_s;

    -- update almost full flag
    almost_full <= '1' when count >= depth-almost_full_space else '0';
    
    -- determine valid write and read
    valid_wr <= wr and not full_s;
    valid_rd <= rd and not empty_s;

    -- update count based on read and write signals
    process(valid_rd, valid_wr, count)
        variable count_v : unsigned(bitsNeeded(depth)-1 downto 0);
    begin

        count_v := count;

        if (valid_rd = '1' and valid_wr = '0') then
            count_v := count_v - 1;
        elsif (valid_rd = '0' and valid_wr = '1') then
            count_v := count_v + 1;
        end if;

        next_count <= count_v;
    end process;

    -- create count register
    process(clk, rst)
    begin
        if (rst = '1') then
            count <= to_unsigned(0, count'length);
        elsif (clk'event and clk = '1') then
            count <= next_count;
        end if;
    end process;
    
end FF;


architecture MEMORY of fifo_core is

    signal rd_addr          : unsigned(bitsNeeded(depth-1)-1 downto 0);
    signal rd_addr_adjusted : unsigned(bitsNeeded(depth-1)-1 downto 0);
    signal wr_addr          : unsigned(bitsNeeded(depth-1)-1 downto 0);
    signal ram_out          : std_logic_vector(width-1 downto 0);

    signal valid_wr : std_logic;
    signal valid_rd : std_logic;
    signal empty_s  : std_logic;
    signal full_s   : std_logic;
    
begin

    -- implement FIFO using BRAM
    BRAM : if use_bram = true generate
        
        SAME_CYCLE : if same_cycle_output = true generate

            -- adjust the rd address to account for the one cycle delay in the
            -- block ram. This ensures that the correct output remains until
            -- the the next read. When a read does occur, this reads the next
            -- location in memory to ensure that data is available on the next
            -- cycle.
            rd_addr_adjusted <= rd_addr when valid_rd = '0' else rd_addr+1;

            -- use RAM with synchronous read during write. This is necessary to
            -- ensure that an output is available 1 cycle after a write, which
            -- is the same time that the empty flag is cleared.
            U_RAM : entity work.ram(SYNC_READ_DURING_WRITE)
                generic map (
                    word_width => width,
                    addr_width => bitsNeeded(depth-1),
                    num_words  => 2**bitsNeeded(depth-1))
                port map (
                    clk   => clk,
                    wen   => valid_wr,
                    waddr => std_logic_vector(wr_addr),
                    wdata => input,
                    raddr => std_logic_vector(rd_addr_adjusted),
                    rdata => ram_out);

            -- avoid warning about unused signal
            output <= ram_out;
            
        end generate SAME_CYCLE;

        NOT_SAME_CYCLE : if same_cycle_output = false generate

            -- avoids warning about unused signal for these generics
            rd_addr_adjusted <= rd_addr;

            -- use RAM with synchronous reads
            U_RAM : entity work.ram(SYNC_READ)
                generic map (
                    word_width => width,
                    addr_width => bitsNeeded(depth-1),
                    num_words  => 2**bitsNeeded(depth-1))
                port map (
                    clk   => clk,
                    wen   => valid_wr,
                    waddr => std_logic_vector(wr_addr),
                    wdata => input,
                    raddr => std_logic_vector(rd_addr_adjusted),
                    rdata => ram_out);

            -- avoid warning about unused signal
            output <= ram_out;
            
        end generate NOT_SAME_CYCLE;
    end generate BRAM;

    -- implement FIFO using distributed RAM, LUTs, or any RAM that supports
    -- asynchronous reads
    -- synthesis tools might convert this to another resource if FPGA memory
    -- does not support asynchronous reads
    DIST_RAM : if use_bram = false generate
        
        SAME_CYCLE : if same_cycle_output = true generate

            -- avoids warning about unused signal for these generics
            rd_addr_adjusted <= rd_addr;

            -- use RAM with asynchronous reads (not supported by all FPGAs)
            U_RAM : entity work.ram(ASYNC_READ)
                generic map (
                    word_width => width,
                    addr_width => bitsNeeded(depth-1),
                    num_words  => 2**bitsNeeded(depth-1))
                port map (
                    clk   => clk,
                    wen   => valid_wr,
                    waddr => std_logic_vector(wr_addr),
                    wdata => input,
                    raddr => std_logic_vector(rd_addr_adjusted),
                    rdata => ram_out);

            -- avoids warning about unused signal for these generics
            output <= ram_out;
            
        end generate SAME_CYCLE;

        NOT_SAME_CYCLE : if same_cycle_output = false generate

            -- avoids warning about unused signal for these generics
            rd_addr_adjusted <= rd_addr;

            -- use RAM with asynchronous reads (not supported by all FPGAs)
            U_RAM : entity work.ram(ASYNC_READ)
                generic map (
                    word_width => width,
                    addr_width => bitsNeeded(depth-1),
                    num_words  => 2**bitsNeeded(depth-1))
                port map (
                    clk   => clk,
                    wen   => valid_wr,
                    waddr => std_logic_vector(wr_addr),
                    wdata => input,
                    raddr => std_logic_vector(rd_addr_adjusted),
                    rdata => ram_out);

            -- add a register to delay the output by a cycle
            process(clk, rst)
            begin
                if (rst = '1') then
                    output <= (others => '0');
                elsif (rising_edge(clk)) then
                    output <= ram_out;
                end if;
            end process;
            
        end generate NOT_SAME_CYCLE;
    end generate DIST_RAM;

    -- update empty flag
    empty_s <= '1' when wr_addr = rd_addr else '0';
    empty   <= empty_s;

    -- update full flag
    --full_s <= '0' when rd = '1' else
    --          '1' when wr_addr + 1 = rd_addr else '0';
    full_s <= '1' when wr_addr + 1 = rd_addr else '0';
    full <= full_s;

    -- update almost_full flag
    almost_full <= '1' when wr_addr + almost_full_space + 1 = rd_addr else '0';

    -- determine valid write and read
    valid_wr <= wr and not full_s;
    valid_rd <= rd and not empty_s;

    -- update wr and rd addresses
    process(clk, rst)
    begin
        if (rst = '1') then
            wr_addr <= (others => '0');
            rd_addr <= (others => '0');
        elsif rising_edge(clk) then
            if (valid_rd = '1') then
                rd_addr <= rd_addr + 1;
            end if;

            if (valid_wr = '1') then
                wr_addr <= wr_addr + 1;
            end if;
        end if;
    end process;

end MEMORY;

