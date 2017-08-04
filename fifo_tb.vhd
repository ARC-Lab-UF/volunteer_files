library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

entity fifo_tb is
end fifo_tb;

architecture TB of fifo_tb is
    
    constant WIDTH               : positive := 32;
    constant DEPTH               : positive := 5;
    constant USE_BRAM            : boolean  := true;
    constant USE_DISTRIBUTED_RAM : boolean  := false;
    constant SAME_CYCLE_OUTPUT   : boolean  := true;
    
    signal clk    : std_logic                          := '0';
    signal rst    : std_logic                          := '0';
    signal rd     : std_logic                          := '0';
    signal wr     : std_logic                          := '0';
    signal empty  : std_logic;
    signal full   : std_logic;
    signal input  : std_logic_vector(WIDTH-1 downto 0) := (others => '0');
    signal output : std_logic_vector(WIDTH-1 downto 0);

    signal sim_done : std_logic := '0';
    
begin  -- TB

    U_UUT : entity work.fifo
        generic map (
            width               => WIDTH,
            depth               => DEPTH,
            use_bram            => USE_BRAM,
            use_distributed_ram => USE_DISTRIBUTED_RAM,
            same_cycle_output   => SAME_CYCLE_OUTPUT)
        port map (
            clk    => clk,
            rst    => rst,
            rd     => rd,
            wr     => wr,
            empty  => empty,
            full   => full,
            input  => input,
            output => output
            );

    clk <= clk when sim_done = '1' else
           not clk after 5 ns;
    
    process
        variable first : boolean;
    begin
        rst <= '1';
        -- Reset Sequence
        for i in 0 to 3 loop
            wait until rising_edge(clk);
        end loop;
        rst <= '0';

        -- test full
        for i in 0 to 2**bitsNeeded(DEPTH)-1 loop
            input <= std_logic_vector(to_unsigned(i, WIDTH));
            wr    <= '1';
            wait until rising_edge(clk);
        end loop;

        wr <= '0';
        assert(full = '1') report "Error: full not asserted";
        wait until rising_edge(clk);

        rst <= '1';
        wait until rising_edge(clk);
        rst <= '0';

        first := true;
        for i in 0 to DEPTH-1 loop
            input <= std_logic_vector(to_unsigned(i, WIDTH));
            wr    <= '1';
            wait until rising_edge(clk);

            -- skip one cycle to deal with 1-cyle read delay
            if (first = false) then
                assert(empty = '0') report "Error: fifo empty after write";
            end if;
            first := false;
        end loop;

        wr <= '0';
        wait until rising_edge(clk);

        -- write and read simulateously
        first := true;
        for i in 0 to DEPTH-1 loop
            input <= std_logic_vector(to_unsigned(i+DEPTH, WIDTH));
            wr    <= '1';
            rd    <= '1';
            wait until rising_edge(clk);

            -- check output depending on output delay
            if (SAME_CYCLE_OUTPUT = true) then
                assert(output = std_logic_vector(to_unsigned(i, WIDTH))) report "Error: fifo has incorrect output";
            elsif (first = false) then
                assert(output = std_logic_vector(to_unsigned(i-1, WIDTH))) report "Error: fifo has incorrect output";
            end if;
            first := false;
        end loop;

        wr <= '0';
        rd <= '0';
        wait until rising_edge(clk);

        -- read remaining data
        first := true;
        for i in 0 to DEPTH-1 loop
            rd <= '1';
            wait until rising_edge(clk);
            if (SAME_CYCLE_OUTPUT = true) then
                assert(output = std_logic_vector(to_unsigned(i+DEPTH, WIDTH))) report "Error: fifo has incorrect output";
            elsif (first = false) then
                assert(output = std_logic_vector(to_unsigned(i+DEPTH-1, WIDTH))) report "Error: fifo has incorrect output";
            end if;
            first := false;
        end loop;

        rd <= '0';
        wait until rising_edge(clk);
        assert(empty = '1') report "Error: fifo isn't empty";

        sim_done <= '1';
        report "DONE!" severity note;
        wait;
    end process;
end TB;
