-- Greg Stitt
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generics Description
-- num_sets : The number of sets in the cache.
-- word_width : The number of bits in a "word," where a word is the unit of
--              data read from and written to the cache.
-- words_per_block : The number of words in a block. (CURRENTLY NOT SUPPORTED)
-- addr_width : The number of bits in the address being looked up in the cache.
--              Must be >= clog2(num_blocks).
-- associativity : The number of blocks per set.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Description:
-- clk    : Clock input
-- rst    : Reset (active high)
-- en     : Enable input (active high), stalls the pipeline when '0'
-- wr_addr : The address to write data to when handling a miss
-- wr_data : Input for providing data to the cache on a miss
-- wr_en   : Assert to write wr_data to wr_addr in cache
-- rd_addr : The address to read from in the cache
-- rd_data : The output from the cache on a hit
-- rd_data_valid : Asserted when rd_data contains valid data. Will remain
--                 asserted for 1 cycle unless en='0'
-- ready : Asserted (active high) when cache can accept new address requests.
--         When not asserted, all inputs are ignored.
-------------------------------------------------------------------------------

entity cache is
    generic (
        num_sets        : positive := 16;
        word_width      : positive := 8;
        words_per_block : positive := 1;
        addr_width      : positive := 16;
        associativity   : positive := 1);
    port (
        clk           : in  std_logic;
        rst           : in  std_logic;
        en            : in  std_logic;
        rd_addr       : in  std_logic_vector(addr_width-1 downto 0);
        rd_en         : in  std_logic;
        rd_data       : out std_logic_vector(word_width-1 downto 0);
        rd_data_valid : out std_logic;
        ready         : out std_logic;

        -- Signals for handling misses
        wr_addr         : in  std_logic_vector(addr_width-1 downto 0);
        wr_data         : in  std_logic_vector(word_width*words_per_block-1 downto 0);
        wr_way_id       : in  std_logic_vector(bitsNeeded(associativity)-1 downto 0);
        wr_en           : in  std_logic;
        miss            : out std_logic;
        miss_addr       : out std_logic_vector(addr_width-1 downto 0);
        miss_valid_ways : out std_logic_vector(associativity-1 downto 0)
        );    
end cache;


architecture default of cache is

    constant DATA_WIDTH : positive := word_width*words_per_block;
    constant TAG_WIDTH  : positive := addr_width - bitsNeeded(num_sets);
    subtype TAG_RANGE is natural range addr_width-1 downto addr_width-TAG_WIDTH;

    constant BLOCK_WIDTH : positive := 1+TAG_WIDTH+DATA_WIDTH;
    constant SET_WIDTH   : positive := BLOCK_WIDTH*associativity;
    --constant NUM_SETS    : positive := integer(ceil(real(num_sets)/real(associativity)));

    type block_array is array (associativity-1 downto 0) of std_logic_vector(BLOCK_WIDTH-1 downto 0);
    signal ram_out : block_array;
    signal way     : block_array;

    signal rd_addr_delayed  : std_logic_vector(rd_addr'range);
    signal rd_addr_r        : std_logic_vector(rd_addr'range);
    signal wr_data_complete : std_logic_vector(wr_data'length+TAG_WIDTH downto 0);

    signal valid      : std_logic_vector(associativity-1 downto 0);
    signal way_tag_eq : std_logic_vector(associativity-1 downto 0);
    signal way_hit    : std_logic_vector(associativity-1 downto 0);

    type way_data_array is array (associativity-1 downto 0) of std_logic_vector(data_width-1 downto 0);
    signal way_data : way_data_array;

    signal hit           : std_logic;
    signal request_valid : std_logic;
    signal ready_s       : std_logic;

    signal miss_handled : std_logic;
    signal data_ready   : std_logic;

    signal hit_r      : std_logic;
    signal hit_data   : std_logic_vector(rd_data'range);
    signal hit_data_r : std_logic_vector(rd_data'range);

    signal way_wr_en : std_logic_vector(associativity-1 downto 0);
    
begin
    assert(words_per_block = 1) severity failure;

    -- Wwrite to the cache the valid bit, the tag, and the actual wr data
    wr_data_complete <= '1' & wr_addr(TAG_RANGE) & wr_data;

    -- RAM to implement each way of the cache
    U_WAYS : for i in 0 to associativity-1 generate
        way_wr_en(i) <= '1' when wr_en = '1' and wr_way_id = std_logic_vector(to_unsigned(i, wr_way_id'length)) else '0';

        U_RAM : entity work.ram(SYNC_READ)
            generic map(
                num_words  => NUM_SETS,
                word_width => SET_WIDTH,
                addr_width => bitsNeeded(NUM_SETS))
            port map (
                clk   => clk,
                wen   => way_wr_en(i),
                waddr => wr_addr(bitsNeeded(num_sets)-1 downto 0),
                wdata => wr_data_complete,
                raddr => rd_addr(bitsNeeded(num_sets)-1 downto 0),
                rdata => ram_out(i));
    end generate;

    -- Register the RAM output for faster clock
    process(clk, rst)
    begin
        if (rst = '1') then
            for i in 0 to associativity-1 loop
                way(i) <= (others => '0');
            end loop;
        elsif (rising_edge(clk)) then
            if (ready_s = '1') then
                for i in 0 to associativity-1 loop
                    way(i) <= ram_out(i);
                end loop;
            end if;
        end if;
    end process;

    -- Delay the input addr by the latency of the memory
    U_DELAY_ADDR : entity work.delay
        generic map (
            cycles => 2,
            width  => addr_width,
            init   => std_logic_vector(to_unsigned(0, addr_width)))
        port map (
            clk    => clk,
            rst    => rst,
            en     => ready_s,
            input  => rd_addr,
            output => rd_addr_delayed
            );

    -- Delay the input tag by the latency of the memory
    U_DELAY_REQUEST : entity work.delay
        generic map (
            cycles => 2,
            width  => 1,
            init   => "0")
        port map (
            clk       => clk,
            rst       => rst,
            en        => ready_s,
            input(0)  => rd_en,
            output(0) => request_valid
            );

    -- Check all set ways for a hit
    U_CHECK_TAG : for i in 0 to associativity-1 generate
        constant TAG_LSB         : positive := DATA_WIDTH;
        constant TAG_MSB         : positive := TAG_LSB + TAG_WIDTH - 1;
        subtype WAY_TAG_RANGE is natural range TAG_MSB downto TAG_LSB;
        constant WAY_VALID_INDEX : positive := TAG_MSB + 1;

        constant DATA_LSB : natural  := 0;
        constant DATA_MSB : positive := DATA_LSB + DATA_WIDTH - 1;
        subtype WAY_DATA_RANGE is natural range DATA_MSB downto DATA_LSB;
    begin
        way_tag_eq(i) <= '1' when way(i)(WAY_TAG_RANGE) = rd_addr_delayed(TAG_RANGE) else '0';
        valid(i)      <= way(i)(WAY_VALID_INDEX);
        way_hit(i)    <= way_tag_eq(i) and valid(i);
        way_data(i)   <= way(i)(WAY_DATA_RANGE);
    end generate;

    -- Or all the way hits together to determine an overall hit
    -- NOTE: Will need to be pipelined for large associativities.
    process(way_hit)
        variable temp_hit : std_logic;
    begin
        temp_hit := way_hit(0);
        for i in 0 to associativity-1 loop
            temp_hit := temp_hit or way_hit(i);
        end loop;

        hit <= temp_hit;
    end process;

    -- Use a mux to select the right way for the output.
    -- NOTE: Will need to be pipelined for large associativities.
    process(way_hit, way_data)
    begin
        hit_data <= way_data(0);
        for i in 0 to associativity-1 loop
            if (way_hit(i) = '1') then
                hit_data <= way_data(i);
            end if;
        end loop;
    end process;

    -------------------------------------------------------------------
    -- Add an extra pipeline stage for handling misses

    -- Registers for handling miss
    process(clk, rst)
    begin
        if (rst = '1') then
            hit_r      <= '0';
            hit_data_r <= (others => '0');
            rd_addr_r  <= (others => '0');
        elsif (rising_edge(clk)) then
            if (ready_s = '1') then
                hit_r           <= hit;
                hit_data_r      <= hit_data;
                rd_addr_r       <= rd_addr_delayed;
                miss_valid_ways <= valid;
            end if;
        end if;
    end process;

    -- A miss has been handled when there is a write to the originally
    -- requested address.
    -- NOTE: might need to be pipelined
    miss_handled <= '1' when wr_addr = rd_addr_r and wr_en = '1' else '0';
    miss         <= not hit_r;
    miss_addr    <= rd_addr_r;

    -- Data is ready to be output when there was a hit, or when the miss has
    -- been handled by an external write.
    data_ready    <= hit_r or miss_handled;
    rd_data_valid <= data_ready;

    -- Mux to select cache data (for a hit) or external data (for a miss)
    rd_data <= hit_data_r when hit_r = '1' else wr_data;

    -- The cache is ready when it is enabled and either there is not currently
    -- a valid request, or the data is ready on a valid request.
    ready_s <= en and (not request_valid or (data_ready and request_valid));
    ready   <= ready_s;

end default;
