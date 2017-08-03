library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity right_shift is
    generic (
        SHIFT_BITS : positive := 1;
        WORD_WIDTH : positive := 8;
        NUM_WORDS  : positive := 2);
    port (
        clk          : in  std_logic;
        rst          : in  std_logic;
        en           : in  std_logic := '1';
        input        : in  std_logic_vector(WORD_WIDTH*NUM_WORDS-1 downto 0);
        shift_amount : in  std_logic_vector(SHIFT_BITS-1 downto 0);
        output       : out std_logic_vector(WORD_WIDTH*NUM_WORDS-1 downto 0);
        valid_in     : in  std_logic;
        valid_out    : out std_logic);
end right_shift;

architecture DEFAULT of right_shift is

    constant LEVELS : positive := SHIFT_BITS;
    type word_array is array (0 to LEVELS-1, 0 to NUM_WORDS-1) of std_logic_vector(WORD_WIDTH-1 downto 0);
    signal words    : word_array;

    type input_array is array (0 to NUM_WORDS-1) of std_logic_vector(WORD_WIDTH-1 downto 0);
    signal inputs : input_array;

    type shift_array is array (0 to LEVELS-1) of std_logic_vector(SHIFT_BITS-1 downto 0);
    signal shift : shift_array;
    
begin

    -- convert input vector into 2d array
    process(input)
    begin
        for i in 0 to NUM_WORDS-1 loop
            inputs(i) <= input(input'length-i*WORD_WIDTH-1 downto input'length-(i+1)*WORD_WIDTH);
        end loop;
    end process;

    process(clk, rst)
    begin
        if (rst = '1') then
            for i in 0 to LEVELS-1 loop
                for j in 0 to NUM_WORDS-1 loop
                    words(i, j) <= (others => '0');
                end loop;
                
                shift(i) <= (others => '0');                
            end loop;
        elsif (rising_edge(clk)) then

            -- level 0
            for j in 0 to NUM_WORDS-1 loop
                if (en = '1') then
                    if (shift_amount(LEVELS-1) = '1') then
                        if (j-2**(LEVELS-1) < 0) then
                            words(0, j) <= (others => '0');
                        else
                            words(0, j) <= inputs(j-2**(LEVELS-1));
                        end if;
                    else
                        words(0, j) <= inputs(j);
                    end if;

                    shift(0) <= shift_amount;
                end if;
            end loop;

            for i in 1 to LEVELS-1 loop
                
                shift(i) <= shift(i-1);                   

                for j in 0 to NUM_WORDS-1 loop
                    if (en = '1') then
                        if (shift(i-1)(LEVELS-i-1) = '1') then
                            -- if word to shift from doesn't exist, shift in 0
                            -- else, shift in words based on the level
                            if (j-2**(LEVELS-i-1) < 0) then
                                words(i, j) <= (others => '0');
                            else
                                words(i, j) <= words(i-1, j-2**(LEVELS-i-1));
                            end if;
                        else
                            words(i, j) <= words(i-1, j);
                        end if;
                    end if;
                end loop;
            end loop;
        end if;
    end process;

    process(words)
    begin
        for i in 0 to NUM_WORDS-1 loop
            output((NUM_WORDS-i)*WORD_WIDTH-1 downto (NUM_WORDS-i-1)*WORD_WIDTH) <= words(LEVELS-1, i);
        end loop;
    end process;

    U_DELAY : entity work.delay
        generic map (
            width  => 1,
            cycles => LEVELS,
            init   => "0")
        port map (
            clk       => clk,
            rst       => rst,
            en        => en,
            input(0)  => valid_in,
            output(0) => valid_out);    

end DEFAULT;
