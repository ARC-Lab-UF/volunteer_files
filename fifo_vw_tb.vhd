-- fifo_vw testbench
-- Greg Stitt
-- University of Florida


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;
use work.tb_pkg.all;

entity fifo_vw_tb is
    generic(
        parallel_io : positive := 4;
        data_width  : positive := 8;
        test_size   : positive := 400;

        input_delay_prob : real range 0.0 to 1.0 := 0.1;
        min_input_delay  : natural               := 1;
        max_input_delay  : natural               := 10;

        output_delay_prob : real range 0.0 to 1.0 := 0.0;
        min_output_delay  : natural               := 1;
        max_output_delay  : natural               := 10
        );
end fifo_vw_tb;


architecture TB of fifo_vw_tb is

    -- ensure that that input size is a multiple of parallel_io. This will
    -- round up test_size to the next multiple
    constant adjusted_input_size : integer := integer(ceil(real(test_size) / real(parallel_io)))*parallel_io;

    type data_array is array (0 to adjusted_input_size-1) of std_logic_vector(data_width-1 downto 0);
    signal input_array      : data_array;
    signal output_array     : data_array;
    signal clk_en           : std_logic                                            := '1';
    signal clk, rst, rd, wr : std_logic                                            := '0';
    signal empty, full      : std_logic;
    signal wr_amount        : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0) := (others => '0');
    signal valid_count      : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0) := (others => '0');
    signal input            : std_logic_vector(parallel_io*data_width-1 downto 0)  := (others => '0');
    signal output           : std_logic_vector(parallel_io*data_width-1 downto 0);

    signal input_ready  : std_logic := '0';
    signal output_ready : std_logic := '0';
    signal input_done   : std_logic := '0';
    signal all_outputs  : boolean   := false;

    type count_array is array (0 to adjusted_input_size-1) of integer;
    signal counts : count_array;

    signal stall : std_logic := '0';
    
begin

    U_FIFO_VW : entity work.fifo_vw
        generic map(
            data_width  => data_width,
            parallel_io => parallel_io)
        port map(
            clk         => clk,
            rst         => rst,
            rd          => rd,
            wr          => wr,
            valid_count => valid_count,
            empty       => empty,
            full        => full,
            input       => input,
            output      => output
            );

    clk <= not clk after 10 ns when clk_en = '1' else '0';

    -- determine when to write
    process(full, input_ready, rst, input_done)
    begin
        -- input_ready includes random delays
        wr <= input_ready and not full and not rst and not input_done;
    end process;

    -- set the inputs during a write
    process(clk, wr, rst)
        variable input_count : integer := 0;
    begin
        if (rst = '1') then
            input_count := 0;

            for j in 0 to parallel_io-1 loop
                input((parallel_io-j)*data_width-1 downto (parallel_io-j-1)*data_width) <= input_array(j);
            end loop;

        -- set the inputs for each write
        elsif (rising_edge(clk) and wr = '1' and rst = '0') then

            input_count := input_count + to_integer(unsigned(valid_count));
            
            -- write valid_count elements into the fifo
            for j in 0 to parallel_io-1 loop
                if (input_count + j < adjusted_input_size) then
                    input((parallel_io-j)*data_width-1 downto (parallel_io-j-1)*data_width) <= input_array(input_count + j);
                end if;
            end loop;

            if (input_count = adjusted_input_size) then
                input_done <= '1';
            end if;
        end if;
    end process;

    -- randomly vary write timings
    process
        variable s1, s2 : positive;     -- seeds for rand function
    begin
        input_ready <= '0';
        wait until rst = '0';
        for i in 0 to 5 loop
            wait until rising_edge(clk);
        end loop;

        while (input_done = '0') loop
            input_ready <= '0';
            --randomize a delay so that the fifo can drain and reads larger than whats in the buffer can be tested
            randDelay(s1, s2, clk, input_delay_prob, min_input_delay, max_input_delay);
            input_ready <= '1';
            wait until rising_edge(clk);
        end loop;

        input_ready <= '0';
        wait;
    end process;

    -- initialize the simulation
    process
        variable rand        : integer;  -- random inputs to be written to fifo
        variable s1, s2      : positive;  -- seeds for rand function
        variable input_count : integer;
    begin

        -- initialize inputs
        for i in 0 to adjusted_input_size-1 loop
--            randomInt(s1, s2, 0, 2**data_width-1, rand);
--            input_array(i) <= std_logic_vector(to_unsigned(rand, data_width));
            input_array(i) <= std_logic_vector(to_unsigned(i, data_width));
        end loop;

        --Reset the entity and initialize the wr and input to zero
        rst <= '1';
        for i in 0 to 5 loop
            wait until rising_edge(clk);
        end loop;

        rst <= '0';
        for i in 0 to 5 loop
            wait until rising_edge(clk);
        end loop;

        wait;
    end process;

    -- randomly vary read timings
    vary_read_timing : process
        variable s1, s2 : positive;     -- seeds for rand function
    begin
        while (not all_outputs) loop
            output_ready <= '0';
            --randomize a delay so that the fifo can drain and reads larger than whats in the buffer can be tested
            randDelay(s1, s2, clk, output_delay_prob, min_output_delay, max_output_delay);
            output_ready <= '1';
            wait until rising_edge(clk);
        end loop;
        wait;
    end process;

    -- determine when reads occur
    process(empty, output_ready)
    begin
        rd <= not empty and output_ready;
    end process;

    -- set write amounts   
    write_amounts : process(clk, rst)
        variable s1, s2      : positive;
        variable wr_amount_v : integer;
    begin
        if (rst = '1') then          
            randomInt(s1, s2, 0, parallel_io+1, wr_amount_v);
            valid_count <= std_logic_vector(to_unsigned(wr_amount_v, valid_count'length));                
        elsif (rising_edge(clk) and wr = '1') then
            randomInt(s1, s2, 0, parallel_io+1, wr_amount_v);
            valid_count <= std_logic_vector(to_unsigned(wr_amount_v, valid_count'length));
        end if;
    end process;

    check_outputs : process(clk)
        variable out_ind_count : integer                                 := 0;
        variable output_temp   : std_logic_vector(data_width-1 downto 0) := (others => '0');
    begin
        -- if there is a valid output, verify it
        if (rising_edge(clk) and rd = '1') then

            -- check the outputs
            for i in 0 to parallel_io-1 loop
                output_temp   := output((parallel_io-i)*data_width-1 downto (parallel_io-i-1)*data_width);
                assert(output_temp = input_array(out_ind_count)) report "Output incorrect.";
                out_ind_count := out_ind_count + 1;
            end loop;
        end if;

        if (out_ind_count = adjusted_input_size) then
            report "SIMULATION COMPLETE!!!";
            clk_en <= '0';
        end if;
    end process;

end TB;
