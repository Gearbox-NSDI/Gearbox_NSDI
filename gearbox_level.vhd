-- gearbox_level module


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use IEEE.math_real."ceil";
use IEEE.math_real."log2";
library xpm;
use xpm.vcomponents.all;
use work.gb_package.all;

entity gearbox_level is
  generic (
    g_FIFO_NUM        : integer;     -- number of FIFOs
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_L2_FIFO_SIZE    : integer;     -- log2 size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer;     -- Descriptor width
    g_PKT_CNT_WIDTH   : integer;     -- packet count width
    g_BYTE_CNT_WIDTH  : integer      -- byte count width
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    -- enq i/f
    enq_cmd                          : in  std_logic;
    enq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    enq_desc                         : in  std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
    enq_done                         : out std_logic;

    -- deq i/f
    deq_cmd                          : in  std_logic;
    deq_fifo_index                   : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    deq_desc                         : out std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    -- find earliest fifo i/f
    find_earliest_non_empty_fifo_cmd : in  std_logic;
    current_fifo_index               : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    find_earliest_non_empty_fifo_rsp : out std_logic;
    earliest_fifo_index              : out unsigned(g_L2_FIFO_NUM-1 downto 0);
    earliest_fifo_byte_cnt           : out unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
    current_fifo_byte_cnt            : out unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
    all_fifos_empty                  : out std_logic;
    -- fifo count i/f
    get_fifo_cnts_cmd                : in  std_logic;
    get_fifo_cnts_index              : in  unsigned(g_L2_FIFO_NUM-1 downto 0);
    get_fifo_cnts_rsp                : out std_logic;
    fifo_pkt_cnt                     : out unsigned(g_PKT_CNT_WIDTH-1 downto 0);
    fifo_byte_cnt                    : out unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
    -- level count i/f (always valid)
    level_pkt_cnt                    : out unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
    level_byte_cnt                   : out unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0)

  );
end gearbox_level;

architecture gearbox_level_arch of gearbox_level is

  signal data_valid        : std_logic_vector(g_FIFO_NUM-1 downto 0);
  type   t_dout_array is array(0 to g_FIFO_NUM-1) of std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0); 
  signal dout              : t_dout_array;
  signal empty             : std_logic_vector(g_FIFO_NUM-1 downto 0);     -- FIFO empty indicator arr
  signal full              : std_logic_vector(g_FIFO_NUM-1 downto 0);     -- FIFO full indicator arr
  signal rd_en             : std_logic_vector(g_FIFO_NUM-1 downto 0);
  signal wr_en             : std_logic_vector(g_FIFO_NUM-1 downto 0);
  signal din               : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
  signal deq_fifo_index_d1 : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
  type   t_pkt_cnt_array is array(0 to g_FIFO_NUM-1) of unsigned(g_PKT_CNT_WIDTH-1 downto 0);
  signal enq_fifo_pkt_cnt  : t_pkt_cnt_array;
  signal deq_fifo_pkt_cnt  : t_pkt_cnt_array;
  signal enq_level_pkt_cnt : unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
  signal deq_level_pkt_cnt : unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
  type   t_byte_cnt_array is array(0 to g_FIFO_NUM-1) of unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
  signal enq_fifo_byte_cnt : t_byte_cnt_array;
  signal deq_fifo_byte_cnt : t_byte_cnt_array;
  signal enq_level_byte_cnt: unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);
  signal deq_level_byte_cnt: unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);

begin
  -- Instantiate FIFOs  
  g_GENERATE_FIFOS: for i in 0 to g_FIFO_NUM-1 generate

    -- xpm_fifo_sync: Synchronous FIFO
    -- Xilinx Parameterized Macro, version 2020.1

    xpm_fifo_sync_inst : xpm_fifo_sync
    generic map (
      DOUT_RESET_VALUE    => "0",                  -- String
      ECC_MODE            => "no_ecc",             -- String
      FIFO_MEMORY_TYPE    => "auto",               -- String
      FIFO_READ_LATENCY   => 1,                    -- DECIMAL
      FIFO_WRITE_DEPTH    => 2**g_L2_FIFO_SIZE,    -- DECIMAL
      FULL_RESET_VALUE    => 0,                    -- DECIMAL
      PROG_EMPTY_THRESH   => 10,                   -- DECIMAL
      PROG_FULL_THRESH    => 10,                   -- DECIMAL
      RD_DATA_COUNT_WIDTH => g_L2_FIFO_SIZE+1,     -- DECIMAL
      READ_DATA_WIDTH     => g_DESC_BIT_WIDTH,     -- DECIMAL
      READ_MODE           => "fwft",               -- String
      SIM_ASSERT_CHK      => 0,                    -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      USE_ADV_FEATURES    => "1404",               -- String
      WAKEUP_TIME         => 0,                    -- DECIMAL
      WRITE_DATA_WIDTH    => g_DESC_BIT_WIDTH,     -- DECIMAL
      WR_DATA_COUNT_WIDTH => g_L2_FIFO_SIZE+1      -- DECIMAL
    )
    port map (
      almost_empty  => open,             -- 1-bit output: Almost Empty : When asserted, this signal indicates that
                                         -- only one more read can be performed before the FIFO goes to empty.

      almost_full   => open,             -- 1-bit output: Almost Full: When asserted, this signal indicates that
                                         -- only one more write can be performed before the FIFO is full.

      data_valid    => data_valid(i),    -- 1-bit output: Read Data Valid: When asserted, this signal indicates
                                         -- that valid data is available on the output bus (dout).

      dbiterr       => open,             -- 1-bit output: Double Bit Error: Indicates that the ECC decoder
                                         -- detected a double-bit error and data in the FIFO core is corrupted.

      dout          => dout(i),          -- READ_DATA_WIDTH-bit output: Read Data: The output data bus is driven
                                         -- when reading the FIFO.

      empty         => empty(i),         -- 1-bit output: Empty Flag: When asserted, this signal indicates that
                                         -- the FIFO is empty. Read requests are ignored when the FIFO is empty,
                                         -- initiating a read while empty is not destructive to the FIFO.

      full          => full(i),          -- 1-bit output: Full Flag: When asserted, this signal indicates that the
                                         -- FIFO is full. Write requests are ignored when the FIFO is full,
                                         -- initiating a write when the FIFO is full is not destructive to the
                                         -- contents of the FIFO.

      overflow      => open,             -- 1-bit output: Overflow: This signal indicates that a write request
                                         -- (wren) during the prior clock cycle was rejected, because the FIFO is
                                         -- full. Overflowing the FIFO is not destructive to the contents of the
                                         -- FIFO.

      prog_empty    => open,             -- 1-bit output: Programmable Empty: This signal is asserted when the
                                         -- number of words in the FIFO is less than or equal to the programmable
                                         -- empty threshold value. It is de-asserted when the number of words in
                                         -- the FIFO exceeds the programmable empty threshold value.

      prog_full     => open,             -- 1-bit output: Programmable Full: This signal is asserted when the
                                         -- number of words in the FIFO is greater than or equal to the
                                         -- programmable full threshold value. It is de-asserted when the number
                                         -- of words in the FIFO is less than the programmable full threshold
                                         -- value.

      rd_data_count => open,             -- RD_DATA_COUNT_WIDTH-bit output: Read Data Count: This bus indicates
                                         -- the number of words read from the FIFO.

      rd_rst_busy   => open,             -- 1-bit output: Read Reset Busy: Active-High indicator that the FIFO
                                         -- read domain is currently in a reset state.

      sbiterr       => open,             -- 1-bit output: Single Bit Error: Indicates that the ECC decoder
                                         -- detected and fixed a single-bit error.

      underflow     => open,             -- 1-bit output: Underflow: Indicates that the read request (rd_en)
                                         -- during the previous clock cycle was rejected because the FIFO is
                                         -- empty. Under flowing the FIFO is not destructive to the FIFO.

      wr_ack        => open,             -- 1-bit output: Write Acknowledge: This signal indicates that a write
                                         -- request (wr_en) during the prior clock cycle is succeeded.

      wr_data_count => open,             -- WR_DATA_COUNT_WIDTH-bit output: Write Data Count: This bus indicates
                                         -- the number of words written into the FIFO.

      wr_rst_busy   => open,             -- 1-bit output: Write Reset Busy: Active-High indicator that the FIFO
                                         -- write domain is currently in a reset state.

      din           => din,              -- WRITE_DATA_WIDTH-bit input: Write Data: The input data bus used when
                                         -- writing the FIFO.

      injectdbiterr => '0',              -- 1-bit input: Double Bit Error Injection: Injects a double bit error if
                                         -- the ECC feature is used on block RAMs or UltraRAM macros.

      injectsbiterr => '0',              -- 1-bit input: Single Bit Error Injection: Injects a single bit error if
                                         -- the ECC feature is used on block RAMs or UltraRAM macros.

      rd_en         => rd_en(i),         -- 1-bit input: Read Enable: If the FIFO is not empty, asserting this
                                         -- signal causes data (on dout) to be read from the FIFO. Must be held
                                         -- active-low when rd_rst_busy is active high.

      rst           => rst,              -- 1-bit input: Reset: Must be synchronous to wr_clk. The clock(s) can be
                                         -- unstable at the time of applying reset, but reset must be released
                                         -- only after the clock(s) is/are stable.

      sleep         => '0',              -- 1-bit input: Dynamic power saving- If sleep is High, the memory/fifo
                                         -- block is in power saving mode.

      wr_clk        => clk,              -- 1-bit input: Write clock: Used for write operation. wr_clk must be a
                                         -- free running clock.

      wr_en         => wr_en(i)          -- 1-bit input: Write Enable: If the FIFO is not full, asserting this
                                         -- signal causes data (on din) to be written to the FIFO Must be held
                                         -- active-low when rst or wr_rst_busy or rd_rst_busy is active high

    );

  end generate g_GENERATE_FIFOS;
  
  -- find earliest non-empty FIFO
  p_earliest_non_empty_fifo: process(rst, clk)
  variable v_earliest_found_left  : boolean := false;
  variable v_earliest_found_right : boolean := false;
  variable v_earliest_fifo_index  : integer range 0 to g_FIFO_NUM-1 := 0;
  begin
    if rst = '1' then
      v_earliest_found_left  := false;
      v_earliest_found_right := false;  
      v_earliest_fifo_index  := 0;
      find_earliest_non_empty_fifo_rsp <= '0';
      all_fifos_empty                  <= '0';
    elsif clk'event and clk = '1' then
      if find_earliest_non_empty_fifo_cmd = '1' then
        v_earliest_found_left  := false;
        v_earliest_found_right := false;
        for i in 0 to g_FIFO_NUM-1 loop
          if i < to_integer(current_fifo_index) then
            if empty(i) = '0' and not v_earliest_found_left then
              v_earliest_fifo_index := i;
              v_earliest_found_left := true;
            end if;
          else
            if empty(i) = '0' and not v_earliest_found_right then
              v_earliest_fifo_index := i;
              v_earliest_found_right := true;
            end if;
          end if;
        end loop;
      end if;
      find_earliest_non_empty_fifo_rsp <= find_earliest_non_empty_fifo_cmd;
      earliest_fifo_index              <= to_unsigned(v_earliest_fifo_index, earliest_fifo_index'length);
      earliest_fifo_byte_cnt           <= enq_fifo_byte_cnt(v_earliest_fifo_index) - 
                                            deq_fifo_byte_cnt(v_earliest_fifo_index);
      current_fifo_byte_cnt            <= enq_fifo_byte_cnt(to_integer(current_fifo_index)) - 
                                            deq_fifo_byte_cnt(to_integer(current_fifo_index));
      all_fifos_empty                  <= '1' when (not v_earliest_found_left and not v_earliest_found_right) else
                                          '0';
    end if;
  end process p_earliest_non_empty_fifo;

  -- Enqueue process
  p_enqueue: process(rst, clk)
  variable v_byte_cnt : unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
  begin
    if rst = '1' then
      wr_en              <= (others=>'0');
      enq_fifo_pkt_cnt   <= (others => (others => '0'));
      enq_fifo_byte_cnt  <= (others => (others => '0'));
      enq_level_pkt_cnt  <= (others => '0');
      enq_level_byte_cnt <= (others => '0');
      enq_done           <= '0'; 
    elsif clk'event and clk = '1' then
      -- defaults
      wr_en      <= (others=>'0');
      enq_done   <= '0';
      
      -- clock 1
      -- wait for enqueue command and latch incoming descriptor
      if enq_cmd = '1' then
        din <= enq_desc;
        wr_en(to_integer(enq_fifo_index)) <= '1';
        
        -- Update enq FIFO and level packet and byte counts
        v_byte_cnt := resize(unsigned(enq_desc(g_DESC_BIT_WIDTH - 1 downto g_DESC_BIT_WIDTH-PKT_LEN_BIT_WIDTH)), v_byte_cnt'length);
        enq_fifo_pkt_cnt(to_integer(enq_fifo_index))  <= enq_fifo_pkt_cnt(to_integer(enq_fifo_index)) + 1;
        enq_fifo_byte_cnt(to_integer(enq_fifo_index)) <= enq_fifo_byte_cnt(to_integer(enq_fifo_index)) + v_byte_cnt;
        enq_level_pkt_cnt                             <= enq_level_pkt_cnt + 1;
        enq_level_byte_cnt                            <= enq_level_byte_cnt + v_byte_cnt;
        
        enq_done <= '1';
      end if;  
    end if;
  end process p_enqueue;
  
  -- dequeue process
  p_dequeue: process(rst, clk)
  begin
    if rst = '1' then
      rd_en <= (others => '0');
      deq_desc_valid     <= '0';    
      deq_fifo_pkt_cnt   <= (others => (others => '0'));
      deq_fifo_byte_cnt  <= (others => (others => '0'));
      deq_level_pkt_cnt  <= (others => '0');
      deq_level_byte_cnt <= (others => '0');
    elsif clk'event and clk = '1' then
      rd_en <= (others => '0');
      deq_desc_valid <= '0';
      
      if deq_cmd = '1' and empty(to_integer(deq_fifo_index)) /= '1' then
        rd_en(to_integer(deq_fifo_index)) <= '1';
        deq_fifo_index_d1 <= deq_fifo_index;
        deq_desc <= dout(to_integer(deq_fifo_index));
        deq_desc_valid <= data_valid(to_integer(deq_fifo_index));
      end if;
      
      if deq_desc_valid = '1' then
        deq_fifo_pkt_cnt(to_integer(deq_fifo_index_d1))  <= deq_fifo_pkt_cnt(to_integer(deq_fifo_index_d1)) + 1;
        deq_fifo_byte_cnt(to_integer(deq_fifo_index_d1)) <= deq_fifo_byte_cnt(to_integer(deq_fifo_index_d1)) + 
                                                          unsigned(deq_desc(g_DESC_BIT_WIDTH - 1 downto g_DESC_BIT_WIDTH-PKT_LEN_BIT_WIDTH));
        deq_level_pkt_cnt                                <= deq_level_pkt_cnt + 1;
        deq_level_byte_cnt                               <= deq_level_byte_cnt + 
                                                          unsigned(deq_desc(g_DESC_BIT_WIDTH - 1 downto g_DESC_BIT_WIDTH-PKT_LEN_BIT_WIDTH));
      end if;
    end if;
  end process p_dequeue;
 
  -- counters process
  p_counters: process(rst, clk)
  begin
    if rst = '1' then
      get_fifo_cnts_rsp <= '0';    
    elsif clk'event and clk = '1' then
      get_fifo_cnts_rsp <= '0';
      if get_fifo_cnts_cmd = '1' then
        fifo_pkt_cnt      <= enq_fifo_pkt_cnt(to_integer(get_fifo_cnts_index)) - 
                             deq_fifo_pkt_cnt(to_integer(get_fifo_cnts_index));
                        
        fifo_byte_cnt     <= enq_fifo_byte_cnt(to_integer(get_fifo_cnts_index)) - 
                             deq_fifo_byte_cnt(to_integer(get_fifo_cnts_index));
        get_fifo_cnts_rsp <= '1';
      end if;
    end if;
  end process p_counters;
  
  -- level count i/f (always valid)
  level_pkt_cnt           <= enq_level_pkt_cnt - deq_level_pkt_cnt;
  level_byte_cnt          <= enq_level_byte_cnt - deq_level_byte_cnt;

end gearbox_level_arch;