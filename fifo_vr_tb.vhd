-- FIFO_vr Testbench
-- Author: Eric Schwartz
-- Greg Stitt
-- University of Florida


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;
use work.tb_pkg.all;

entity fifo_vr_tb is
    generic(
        parallel_io    : positive := 4;
        data_width     : positive := 8;
        input0_at_MSB  : boolean  := true;
        output0_at_MSB : boolean  := false;
        test_size      : positive := 10000;

        input_delay_prob : real range 0.0 to 1.0 := 0.25;
        min_input_delay  : natural               := 1;
        max_input_delay  : natural               := 10;

        output_delay_prob : real range 0.0 to 1.0 := 0.1;
        min_output_delay  : natural               := 1;
        max_output_delay  : natural               := 10;

        stall_prob : real range 0.0 to 1.0 := 0.1;
        min_stall_delay  : natural               := 1;
        max_stall_delay  : natural               := 10
        );
end fifo_vr_tb;


architecture TB of fifo_vr_tb is

    -- ensure that that input size is a multiple of parallel_io. This will
    -- round up test_size to the next multiple
    constant adjusted_input_size : integer := integer(ceil(real(test_size) / real(parallel_io)))*parallel_io;

    type data_array is array (0 to adjusted_input_size-1) of std_logic_vector(data_width-1 downto 0);
    signal input_array      : data_array;
    signal output_array     : data_array;
    signal clk_en           : std_logic                                            := '1';
    signal clk, rst, rd, wr : std_logic                                            := '0';
    signal empty, full      : std_logic;
    signal rd_amount        : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0) := (others => '0');
    signal count            : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
    signal input            : std_logic_vector(parallel_io*data_width-1 downto 0)  := (others => '0');
    signal output           : std_logic_vector(parallel_io*data_width-1 downto 0);
    signal valid_out        : std_logic_vector(parallel_io-1 downto 0);

    signal input_ready : std_logic := '0';
    signal output_ready : std_logic := '0';
    signal input_done  : std_logic := '0';
    signal all_outputs : boolean := false;

    type count_array is array (0 to adjusted_input_size-1) of integer;
    signal counts : count_array;

    signal stall : std_logic := '0';
    
begin

    U_FIFO_VR : entity work.fifo_vr
        generic map(
            data_width     => data_width,
            parallel_io    => parallel_io,
            input0_at_MSB  => input0_at_MSB,
            output0_at_MSB => output0_at_MSB)
        port map(
            clk       => clk,
            rst       => rst,
            rd        => rd,
            rd_amount => rd_amount,
            wr        => wr,
            stall     => stall,
            empty     => empty,
            full      => full,
            input     => input,
            output    => output,
            count     => count,
            valid_out => valid_out
            );

    clk <= not clk after 10 ns when clk_en = '1' else '0';

    -- determine when to write
    process(full, input_ready, rst, input_done)
    begin
        -- input_ready includes random delays
        wr <= input_ready and not full and not rst and not input_done;
    end process;

    -- set the inputs during a writes
    process(clk, wr, rst)
        variable input_count : integer := parallel_io+1;
    begin
        if (rst = '1') then
            if (input0_at_MSB) then
                for j in 0 to parallel_io-1 loop
                    input((parallel_io-j)*data_width-1 downto (parallel_io-j-1)*data_width) <= input_array(j);
                end loop;
            else
                for j in 0 to parallel_io-1 loop
                    input((j+1)*data_width-1 downto j*data_width) <= input_array(j);
                end loop;
            end if;

            input_count := parallel_io;

        -- set the inputs for each write
        elsif (rising_edge(clk) and wr = '1' and rst = '0') then

            if (input_count = adjusted_input_size) then
                input_done <= '1';
            else

                for j in 0 to parallel_io-1 loop
                    if (input_count < adjusted_input_size) then

                        -- set input order depending on configuration
                        if (input0_at_MSB) then
                            input((parallel_io-j)*data_width-1 downto (parallel_io-j-1)*data_width) <= input_array(input_count);
                        else
                            input((j+1)*data_width-1 downto j*data_width) <= input_array(input_count);
                        end if;

                        input_count := input_count + 1;
                    end if;
                end loop;
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

    vary_stall : process
        variable s1, s2 : positive;     -- seeds for rand function
    begin
        while (not all_outputs) loop
            stall <= '1';
            --randomize a delay so that the fifo can drain and reads larger than whats in the buffer can be tested
            randDelay(s1, s2, clk, stall_prob, min_stall_delay, max_stall_delay);
            stall <= '0';
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
    -- TODO: Extend with stall functionality
    process(empty, output_ready)
    begin
        rd <= not empty and output_ready and not stall;
    end process;

    -- set read amounts   
    read_amounts: process
        variable output_count   : integer := 0;
        variable s1, s2         : positive;
        variable rd_amount_v    : integer;
        variable output_advance : integer;
        variable num_reads : integer := 0;
    begin
        -- while there are still outputs to check
        while (output_count < adjusted_input_size) loop

            --randomize the number of elements to be read, up to parallel_io
            randomInt(s1, s2, 0, parallel_io+1, rd_amount_v);
            rd_amount <= std_logic_vector(to_unsigned(rd_amount_v, rd_amount'length));
                       
            -- wait until the data is read
            wait until rising_edge(clk) and rd = '1';

            -- determine how much to advance the output
            -- count is the amount of data that was actually available, so
            -- never advance more than that
            if (unsigned(count) < rd_amount_v) then
                output_advance := to_integer(unsigned(count));
            else
                output_advance := rd_amount_v;
            end if;

            -- Save outputs in output_array
            if (output_advance > 0) then
                -- save the count that was used
                counts(num_reads) <= output_advance;
                num_reads         := num_reads + 1;
            end if;
            
           
            output_count := output_count + output_advance;

        end loop;
        
        wait until all_outputs;
        
        report "SIMULATION COMPLETE!!!";
        clk_en <= '0';
        wait;
    end process;

    check_outputs : process(clk)
        variable current_count  : integer := 0;
        variable out_ind_count : integer := 0;
        variable output_temp : std_logic_vector(data_width-1 downto 0) := (others => '0');
        variable out_group_count  : integer := 0;
    begin
        -- if there is a valid output, verify it
        if (rising_edge(clk) and valid_out /= std_logic_vector(to_unsigned(0, valid_out'length)) and stall = '0') then

            -- get the corresponding count for the read that is currently on
            -- the output
            current_count := counts(out_group_count);

            -- make sure the valid out bits match the count
            for i in 0 to current_count-1 loop
                if (output0_at_MSB) then
                    assert(valid_out(parallel_io-i-1) = '1') report "Valid out not asserted.";
                -- report integer'image(count_temp) & " " & integer'image(to_integer(unsigned(valid_out)));
                else
                    assert(valid_out(i) = '1') report "Valid out not asserted.";
                end if;
            end loop;

            -- check the outputs
            for i in 0 to current_count-1 loop
                if (output0_at_MSB) then
                    output_temp := output((parallel_io-i)*data_width-1 downto (parallel_io-i-1)*data_width);
                else
                    output_temp := output((i+1)*data_width-1 downto i*data_width);                    
                end if;

                assert(output_temp = input_array(out_ind_count)) report "Output incorrect.";
                out_ind_count := out_ind_count + 1;
            end loop;

            out_group_count := out_group_count + 1;
        end if;

        if (out_ind_count = adjusted_input_size) then
            all_outputs <= true;
        end if;
    end process;

end TB;
