-- gearboxI module

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;
use IEEE.math_real."ceil";
use IEEE.math_real."log2";
use work.gb_package.all;

entity gearbox_I is
  generic (
    g_L2_FLOW_NUM     : integer := 10;    -- log2 of number of flows
    g_L2_LEVEL_NUM    : integer := 2;     -- log2 of number of levels
    g_L2_FIFO_NUM     : integer := 4;     -- log2 of number of FIFOs
    g_L2_FIFO_SIZE    : integer := 8;     -- log2 of size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer := 57;    -- Descriptor width
    g_VC_BIT_WIDTH    : integer := 20;    -- number of bits in VC
    g_PKT_CNT_WIDTH   : integer := 9;     -- packet count width
    g_BYTE_CNT_WIDTH  : integer := 20     -- byte count width

  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    
    enq_cmd                          : in  std_logic;
    enq_desc                         : in  std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    enq_done                         : out std_logic;
    
    deq_cmd                          : in  std_logic;
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    
    drop_cmd                         : out std_logic;
    drop_desc                        : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);

    gb_pkt_cnt                       : out unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0)

  );
end gearbox_I;

architecture gearbox_I_arch of gearbox_I is

component gearbox_level
  generic (
    g_FIFO_NUM        : integer;     -- number of FIFOs
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_L2_FIFO_SIZE    : integer;     -- log2 of size (depth) of each FIFO
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
end component;

COMPONENT fin_time_arr
  PORT (
    a    : IN STD_LOGIC_VECTOR(g_L2_FLOW_NUM-1 DOWNTO 0);
    d    : IN STD_LOGIC_VECTOR(g_VC_BIT_WIDTH-1 DOWNTO 0);
    dpra : IN STD_LOGIC_VECTOR(g_L2_FLOW_NUM-1 DOWNTO 0);
    clk  : IN STD_LOGIC;
    we   : IN STD_LOGIC;
    dpo  : OUT STD_LOGIC_VECTOR(g_VC_BIT_WIDTH-1 DOWNTO 0)
  );
END COMPONENT;

COMPONENT last_enq_level_arr
  PORT (
    a   : IN STD_LOGIC_VECTOR(g_L2_FLOW_NUM-1 DOWNTO 0);
    d   : IN STD_LOGIC_VECTOR(g_L2_LEVEL_NUM-1 DOWNTO 0);
    clk : IN STD_LOGIC;
    we  : IN STD_LOGIC;
    spo : OUT STD_LOGIC_VECTOR(g_L2_LEVEL_NUM-1 DOWNTO 0)
  );
END COMPONENT;


-- derived constants
constant g_FLOW_NUM        : integer := 2**g_L2_FLOW_NUM;      -- number of flows
constant g_LEVEL_NUM       : integer := 2**g_L2_LEVEL_NUM;     -- number of levels
constant g_FIFO_NUM        : integer := 2**g_L2_FIFO_NUM;      -- number of FIFOs

-- enqueue signals
--type t_fin_time_arr is array(0 to g_FLOW_NUM-1) of unsigned(g_VC_BIT_WIDTH-1 downto 0);
--signal fin_time_arr : t_fin_time_arr;
type t_current_fifo_arr is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM-1 downto 0);
signal current_fifo_arr: t_current_fifo_arr;
--type t_last_enq_level_arr is array(0 to g_FLOW_NUM-1) of unsigned(g_L2_LEVEL_NUM-1 downto 0);
--signal last_enq_level_arr : t_last_enq_level_arr;

signal enq_cmd_d1          : std_logic;
signal enq_cmd_d2          : std_logic;
signal lvl_enq_cmd_A       : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal lvl_enq_cmd_B       : std_logic_vector(g_LEVEL_NUM-2 downto 0);
signal lvl_enq_fifo_index  : unsigned(g_L2_FIFO_NUM-1 downto 0);
signal lvl_enq_desc        : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
type   t_fifo_pkt_cnt_arr is array(0 to g_LEVEL_NUM-1) of unsigned(g_PKT_CNT_WIDTH-1 downto 0);
signal fifo_pkt_cnt_A      : t_fifo_pkt_cnt_arr;
signal fifo_pkt_cnt_B      : t_fifo_pkt_cnt_arr;
type   t_byte_cnt_arr_A is array(0 to g_LEVEL_NUM-1) of unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
type   t_byte_cnt_arr_B is array(0 to g_LEVEL_NUM-2) of unsigned(g_BYTE_CNT_WIDTH-1 downto 0);
signal fifo_byte_cnt_A     : t_byte_cnt_arr_A;
signal fifo_byte_cnt_B     : t_byte_cnt_arr_B;
signal earliest_fifo_byte_cnt_A : t_byte_cnt_arr_A;
signal earliest_fifo_byte_cnt_B : t_byte_cnt_arr_B;
signal current_fifo_byte_cnt_A  : t_byte_cnt_arr_A;
signal current_fifo_byte_cnt_B  : t_byte_cnt_arr_B;
type   t_level_pkt_cnt_arr_A is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
signal level_pkt_cnt_A     : t_level_pkt_cnt_arr_A;
type   t_level_pkt_cnt_arr_B is array(0 to g_LEVEL_NUM-2) of unsigned(g_L2_FIFO_NUM+g_PKT_CNT_WIDTH-1 downto 0);
signal level_pkt_cnt_B     : t_level_pkt_cnt_arr_B;
type   t_level_byte_cnt_arr_A is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);
signal level_byte_cnt_A    : t_level_byte_cnt_arr_A;
type   t_level_byte_cnt_arr_B is array(0 to g_LEVEL_NUM-2) of unsigned(g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);
signal level_byte_cnt_B    : t_level_byte_cnt_arr_B;
signal gb_enq_pkt_cnt      : unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);
signal flow_id             : unsigned(g_L2_FLOW_NUM-1 downto 0);
signal fin_time            : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0);
signal fin_time_we         : std_logic;
signal fin_time_dpra       : std_logic_vector (g_L2_FLOW_NUM-1 downto 0);
signal fin_time_dpo        : std_logic_vector (FIN_TIME_BIT_WIDTH-1 downto 0);
signal ft_incr             : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0);
signal enq_level           : unsigned(g_L2_LEVEL_NUM-1 downto 0);
signal last_enq_level_spo  : std_logic_vector(g_L2_LEVEL_NUM-1 downto 0);
signal last_enq_level_we   : std_logic;
signal fifo_offset         : unsigned(g_L2_FIFO_NUM-1 downto 0);

-- dequeue signals
type t_deq_state is (IDLE, WAIT_1ST_NON_EMPTY, SERVE_BYTES, WAIT_DEQ_VLD);
signal deq_state           : t_deq_state;
signal vc                  : unsigned(g_VC_BIT_WIDTH-1 downto 0);
signal is_serve_A_arr      : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal lvl_deq_cmd_A       : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal lvl_deq_cmd_B       : std_logic_vector(g_LEVEL_NUM-2 downto 0);
signal lvl_deq_fifo_index  : unsigned(g_L2_FIFO_NUM-1 downto 0);
type   t_lvl_deq_fifo_index_arr_A is array(0 to g_LEVEL_NUM-1) of std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
signal lvl_deq_desc_A      : t_lvl_deq_fifo_index_arr_A;
type   t_lvl_deq_fifo_index_arr_B is array(0 to g_LEVEL_NUM-1) of std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0);
signal lvl_deq_desc_B      : t_lvl_deq_fifo_index_arr_B;
signal lvl_deq_desc_valid_A: std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal lvl_deq_desc_valid_B: std_logic_vector(g_LEVEL_NUM-2 downto 0);
signal find_earliest_non_empty_fifo_cmd_A: std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal find_earliest_non_empty_fifo_cmd_B: std_logic_vector(g_LEVEL_NUM-2 downto 0);
signal find_earliest_non_empty_fifo_rsp_A: std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal find_earliest_non_empty_fifo_rsp_B: std_logic_vector(g_LEVEL_NUM-2 downto 0);
type   t_earliest_fifo_index_arr_A is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM-1 downto 0);
signal earliest_fifo_index_A             : t_earliest_fifo_index_arr_A;
type   t_earliest_fifo_index_arr_B is array(0 to g_LEVEL_NUM-2) of unsigned(g_L2_FIFO_NUM-1 downto 0);
signal earliest_fifo_index_B             : t_earliest_fifo_index_arr_B;
signal all_fifos_empty_A                 : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal all_fifos_empty_B                 : std_logic_vector(g_LEVEL_NUM-2 downto 0);
signal get_fifo_cnts_cmd_A               : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal get_fifo_cnts_cmd_B               : std_logic_vector(g_LEVEL_NUM-2 downto 0);
type   t_get_fifo_cnts_index_A is array(0 to g_LEVEL_NUM-1) of unsigned(g_L2_FIFO_NUM-1 downto 0);
signal get_fifo_cnts_index_A             : t_get_fifo_cnts_index_A;
type   t_get_fifo_cnts_index_B is array(0 to g_LEVEL_NUM-2) of unsigned(g_L2_FIFO_NUM-1 downto 0);
signal get_fifo_cnts_index_B             : t_get_fifo_cnts_index_B;
signal get_fifo_cnts_rsp_A               : std_logic_vector(g_LEVEL_NUM-1 downto 0);
signal get_fifo_cnts_rsp_B               : std_logic_vector(g_LEVEL_NUM-2 downto 0);
type t_bytes_to_serve is array(0 to g_LEVEL_NUM-1) of signed(g_BYTE_CNT_WIDTH-1 downto 0);
signal bytes_to_serve                    : t_bytes_to_serve;
--signal bytes_served                      : t_byte_cnt_arr_A;
signal deqd_level                        : unsigned(g_LEVEL_NUM-1 downto 0);
signal gb_deq_pkt_cnt                    : unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0);

begin

  -- <gearbox_levels arr> level_set_A[level_num]
  -- Instantiate level set A  
  g_GENERATE_LEVELS_A: for i in 0 to g_LEVEL_NUM-1 generate
    i_gb_level_a: gearbox_level
    generic map(
      g_FIFO_NUM        => g_FIFO_NUM,       -- number of FIFOs
      g_L2_FIFO_NUM     => g_L2_FIFO_NUM,    -- log2 of number of FIFOs
      g_L2_FIFO_SIZE    => g_L2_FIFO_SIZE,   -- log2 of size (depth) of each FIFO
      g_DESC_BIT_WIDTH  => g_DESC_BIT_WIDTH, -- Descriptor width
      g_PKT_CNT_WIDTH   => g_PKT_CNT_WIDTH,  -- packet count width
      g_BYTE_CNT_WIDTH  => g_BYTE_CNT_WIDTH  -- byte count width
    )
    port map (
      rst                              => rst,
      clk                              => clk,
      -- enq i/f
      enq_cmd                          => lvl_enq_cmd_A(i),
      enq_fifo_index                   => lvl_enq_fifo_index,
      enq_desc                         => lvl_enq_desc,
      enq_done                         => open,
      -- deq i/f
      deq_cmd                          => lvl_deq_cmd_A(i),
      deq_fifo_index                   => lvl_deq_fifo_index,
      deq_desc                         => lvl_deq_desc_A(i),
      deq_desc_valid                   => lvl_deq_desc_valid_A(i),
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_A(i),
      current_fifo_index               => current_fifo_arr(i),
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_A(i),
      earliest_fifo_index              => earliest_fifo_index_A(i),
      earliest_fifo_byte_cnt           => earliest_fifo_byte_cnt_A(i),
      current_fifo_byte_cnt            => current_fifo_byte_cnt_A(i),
      all_fifos_empty                  => all_fifos_empty_A(i),
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_A(i),
      get_fifo_cnts_index              => get_fifo_cnts_index_A(i),
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_A(i),
      fifo_pkt_cnt                     => fifo_pkt_cnt_A(i),
      fifo_byte_cnt                    => fifo_byte_cnt_A(i),
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_A(i),
      level_byte_cnt                   => level_byte_cnt_A(i)
    );
  end generate g_GENERATE_LEVELS_A;

  -- <gearbox_levels arr> level_set_B[level_num - 1]
  -- Instantiate level set B  
  g_GENERATE_LEVELS_B: for i in 0 to g_LEVEL_NUM-2 generate
    i_gb_level_b: gearbox_level
    generic map(
      g_FIFO_NUM        => g_FIFO_NUM,       -- number of FIFOs
      g_L2_FIFO_NUM     => g_L2_FIFO_NUM,    -- log2 of number of FIFOs
      g_L2_FIFO_SIZE    => g_L2_FIFO_SIZE,   -- size (depth) of each FIFO
      g_DESC_BIT_WIDTH  => g_DESC_BIT_WIDTH, -- Descriptor width
      g_PKT_CNT_WIDTH   => g_PKT_CNT_WIDTH,  -- packet count width
      g_BYTE_CNT_WIDTH  => g_BYTE_CNT_WIDTH  -- byte count width
    )
    port map (
      rst                              => rst,
      clk                              => clk,
      -- enq i/f
      enq_cmd                          => lvl_enq_cmd_B(i),
      enq_fifo_index                   => lvl_enq_fifo_index,
      enq_desc                         => lvl_enq_desc,
      enq_done                         => open,
      -- deq i/f
      deq_cmd                          => lvl_deq_cmd_B(i),
      deq_fifo_index                   => lvl_deq_fifo_index,
      deq_desc                         => lvl_deq_desc_B(i),
      deq_desc_valid                   => lvl_deq_desc_valid_B(i),
      -- find earliest fifo i/f
      find_earliest_non_empty_fifo_cmd => find_earliest_non_empty_fifo_cmd_B(i),
      current_fifo_index               => current_fifo_arr(i),
      find_earliest_non_empty_fifo_rsp => find_earliest_non_empty_fifo_rsp_B(i),
      earliest_fifo_index              => earliest_fifo_index_B(i),
      earliest_fifo_byte_cnt           => earliest_fifo_byte_cnt_B(i),
      current_fifo_byte_cnt            => current_fifo_byte_cnt_B(i),
      all_fifos_empty                  => all_fifos_empty_B(i),
      -- fifo count i/f
      get_fifo_cnts_cmd                => get_fifo_cnts_cmd_B(i),
      get_fifo_cnts_index              => get_fifo_cnts_index_B(i),
      get_fifo_cnts_rsp                => get_fifo_cnts_rsp_B(i),
      fifo_pkt_cnt                     => fifo_pkt_cnt_B(i),
      fifo_byte_cnt                    => fifo_byte_cnt_B(i),
      -- level count i/f (always valid)
      level_pkt_cnt                    => level_pkt_cnt_B(i),
      level_byte_cnt                   => level_byte_cnt_B(i)
    );
  end generate g_GENERATE_LEVELS_B;

  -- Finish time table
  fin_time_dpra <= enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - g_L2_FLOW_NUM);
  
  i_fin_time_arr: fin_time_arr
  PORT MAP (
    a    => std_logic_vector(flow_id),
    d    => std_logic_vector(fin_time),
    dpra => fin_time_dpra,
    clk  => clk,
    we   => fin_time_we,
    dpo  => fin_time_dpo
  );
  
  i_last_enq_level_arr : last_enq_level_arr
  PORT MAP (
    a   => std_logic_vector(flow_id),
    d   => std_logic_vector(enq_level),
    clk => clk,
    we  => last_enq_level_we,
    spo => last_enq_level_spo
  );

  -- Enqueue processing 
  p_enqueue: process(rst, clk)
  variable v_enq_level_found: boolean := false;
  variable v_enq_level      : unsigned(g_L2_LEVEL_NUM-1 downto 0) := (others => '0');
  variable v_pkt_time       : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0) := (others => '0');
  variable v_flow_id        : unsigned(g_L2_FLOW_NUM-1 downto 0) := (others => '0');
  variable v_fin_time       : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0) := (others => '0');
  variable v_fifo_offset    : unsigned(g_L2_FIFO_NUM-1 downto 0) := (others => '0');
  begin
    if rst = '1' then
      drop_cmd   <= '0';
      enq_cmd_d1 <= '0';
      enq_cmd_d2 <= '0';
      enq_done   <= '0';
      lvl_enq_cmd_A <= (others => '0');
      lvl_enq_cmd_B <= (others => '0');
--      fin_time_arr  <= (others => (others => '0'));
      fin_time_we   <= '0';
--      last_enq_level_arr <= (others => (others => '0'));
      last_enq_level_we <= '0';
      gb_enq_pkt_cnt     <= (others => '0');
      v_enq_level_found := false;
      
    elsif clk'event and clk = '1' then
      -- defaults
      drop_cmd   <= '0';
      enq_done   <= '0';
      lvl_enq_cmd_A <= (others => '0');
      lvl_enq_cmd_B <= (others => '0');
      enq_cmd_d1 <= enq_cmd;
      enq_cmd_d2 <= enq_cmd_d1;
      fin_time_we   <= '0';
      last_enq_level_we <= '0';

      -- determine current FIFO per level based on virtual clock 
      for i in 0 to g_LEVEL_NUM - 1 loop
        current_fifo_arr(i) <= vc((i+1)*g_L2_FIFO_NUM - 1 downto i*g_L2_FIFO_NUM);
      end loop;

      -- Clk 1: process enqueue command
      if enq_cmd = '1' then
        -- load both drop and level enqueue descriptor - we will use only one of them
        drop_desc    <= enq_desc;
        lvl_enq_desc <= enq_desc;
        -- extract packet time and flow id from descriptor
        v_pkt_time := unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH));
        v_flow_id  := unsigned(enq_desc(g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - 1 downto 
                                        g_DESC_BIT_WIDTH - PKT_LEN_BIT_WIDTH - FIN_TIME_BIT_WIDTH - g_L2_FLOW_NUM));
        flow_id    <= v_flow_id;
        -- calculate incremental and actual finish time
        if vc >= unsigned(fin_time_dpo) then
          ft_incr  <= v_pkt_time;
          fin_time <= v_pkt_time + vc;
        else
          ft_incr  <= v_pkt_time + unsigned(fin_time_dpo) - vc;
          fin_time <= v_pkt_time + unsigned(fin_time_dpo);
        end if;
      end if;
    
      -- Clk 2: Check last enque level of this flow:
      -- If last_enque_level[flow_id] < insert_level:
      --   last_enque_level[flow_id] = insert_level
      -- else:
      --   insert_level = last_enque_level[flow_id]
      if enq_cmd_d1 = '1' then
      
        -- if incremental finish time greater than max level capacity, drop pkt (check MSB bits)
        if ft_incr(FIN_TIME_BIT_WIDTH - 1 downto g_L2_LEVEL_NUM*g_L2_FIFO_NUM) /= to_unsigned(0, g_L2_FIFO_NUM) then
          drop_cmd <= '1';
        else
          -- update fin_time_arr entry for flow_id
          fin_time_we <= '1';
        end if;

        -- determine enq level by examining slices of fin_time.  enq_level = highest non-zero slice
        v_enq_level_found := false;
        for i in g_LEVEL_NUM downto 1 loop
          if ft_incr(i*g_L2_FIFO_NUM - 1 downto (i-1)*g_L2_FIFO_NUM) /= to_unsigned(0, g_L2_FIFO_NUM) and 
               v_enq_level_found = false then
            enq_level <= to_unsigned(i - 1, g_L2_LEVEL_NUM);
            fifo_offset <= ft_incr(i*g_L2_FIFO_NUM - 1 downto (i-1)*g_L2_FIFO_NUM);
            v_enq_level_found := true;
          end if;
        end loop;
      
      end if;

      if enq_cmd_d2 = '1' and drop_cmd = '0' then        
        if enq_level >= unsigned(last_enq_level_spo) then
          last_enq_level_we <= '1';
          v_enq_level := enq_level;
          v_fifo_offset := fifo_offset;
        else
          v_enq_level := unsigned(last_enq_level_spo);
          for i in g_LEVEL_NUM-1 downto 1 loop
            if i = v_enq_level then
              v_fifo_offset := ft_incr((i+1)*g_L2_FIFO_NUM - 1 downto i*g_L2_FIFO_NUM);
            end if;
          end loop;
        end if;
    
        -- Two cases: a) insert level = top level; b) insert level < top level
        -- a) insert top level
        -- Top level only have one set (set A)
        if v_enq_level = g_LEVEL_NUM - 1 then
          lvl_enq_fifo_index             <= current_fifo_arr(to_integer(v_enq_level)) + v_fifo_offset;
          lvl_enq_cmd_A(g_LEVEL_NUM - 1) <= '1';
        else
          -- b) insert level < top level
          -- We have 2 sets (set A & B)
          -- get cur_fifo of current serving set (A or B by is_serve_A[level] array)
          -- if (cur_fifo + offset < fifo_num)
          --   enque current set
          --   enque_fifo_index = curr_fifo + offset
          -- else
          --   enque other set
          --   enque_fifo_index = curr_fifo + offset - fifo_num          
          if is_serve_A_arr(to_integer(v_enq_level)) = '1' then
            if current_fifo_arr(to_integer(v_enq_level)) + fifo_offset < g_FIFO_NUM then
              lvl_enq_fifo_index             <= current_fifo_arr(to_integer(v_enq_level)) + v_fifo_offset;
              lvl_enq_cmd_A(to_integer(v_enq_level)) <= '1';
            else
              lvl_enq_fifo_index             <= current_fifo_arr(to_integer(v_enq_level)) + v_fifo_offset;
              lvl_enq_cmd_B(to_integer(v_enq_level)) <= '1';
            end if;
          else
            if current_fifo_arr(to_integer(v_enq_level)) + fifo_offset < g_FIFO_NUM then
              lvl_enq_fifo_index             <= current_fifo_arr(to_integer(v_enq_level)) + v_fifo_offset;
              lvl_enq_cmd_B(to_integer(v_enq_level)) <= '1';
            else
              lvl_enq_fifo_index             <= current_fifo_arr(to_integer(v_enq_level)) + v_fifo_offset;
              lvl_enq_cmd_A(to_integer(v_enq_level)) <= '1';
            end if;
          end if;
          enq_done <= '1';      
        end if;
        -- increment packet counter
        gb_enq_pkt_cnt <= gb_enq_pkt_cnt + 1;
      end if;
    end if;
  end process p_enqueue;
   
  -- dequeue processing
  -- Issue find earliest non-empty FIFO command asynchronously
  find_earliest_non_empty_fifo_cmd_A <= (others => '1') when deq_cmd = '1' else (others => '0');
  find_earliest_non_empty_fifo_cmd_B <= (others => '1') when deq_cmd = '1' else (others => '0');

  p_dequeue: process(rst, clk)
  variable v_not_served: std_logic_vector(g_LEVEL_NUM-1 downto 0) := (others => '1');
  variable v_deqd_bytes: unsigned(PKT_LEN_BIT_WIDTH-1 downto 0) := (others => '0');
  variable v_more_bytes_to_serve: boolean := false;
  variable v_vc_updated: boolean := false;
  variable v_deq_level: integer := 0;
  begin
    if rst = '1' then
      vc             <= (others => '0');
      is_serve_A_arr <= (others => '1');
      deq_state      <= IDLE;
 --     find_earliest_non_empty_fifo_cmd_A <= (others => '0');
 --     find_earliest_non_empty_fifo_cmd_B <= (others => '0');
      lvl_deq_cmd_A  <= (others => '0');
      lvl_deq_cmd_B  <= (others => '0');
      get_fifo_cnts_cmd_A <= (others => '0');
      get_fifo_cnts_cmd_B <= (others => '0');
      bytes_to_serve <= (others => (others => '0'));
      v_not_served   := (others => '1');
      gb_deq_pkt_cnt <= (others => '0');
      v_more_bytes_to_serve := false;
      v_vc_updated   := false;
      
    elsif clk'event and clk = '1' then
 --     find_earliest_non_empty_fifo_cmd_A <= (others => '0');
 --     find_earliest_non_empty_fifo_cmd_B <= (others => '0');
      lvl_deq_cmd_A <= (others => '0');
      lvl_deq_cmd_B <= (others => '0');
      get_fifo_cnts_cmd_A <= (others => '0');
      get_fifo_cnts_cmd_B <= (others => '0');

      case deq_state is
        when IDLE =>
          -- wait for dequeue signal
          if deq_cmd = '1' then
--            for i in 0 to g_LEVEL_NUM-1 loop
--              find_earliest_non_empty_fifo_cmd_A(i) <= '1';
--              if i < g_LEVEL_NUM-1 then
--                find_earliest_non_empty_fifo_cmd_B(i) <= '1';
--              end if;
--            end loop;
            deq_state <= WAIT_1ST_NON_EMPTY;
          end if;
          
        when WAIT_1ST_NON_EMPTY => 
          v_vc_updated := false;
          for i in 0 to g_LEVEL_NUM-1 loop
            -- find dequeue level
            -- find_earliest_non_empty_fifo_rsp_A and _B arrive simultaneously.  We only check _A
            if find_earliest_non_empty_fifo_rsp_A(i) = '1' then
              if is_serve_A_arr(i) = '1' then 
                if all_fifos_empty_A(i) = '0' then
                  -- update VC slice corresponding to level
                  if not v_vc_updated then
                    if earliest_fifo_index_A(i) > current_fifo_arr(i) + 1 then
                      if i < g_LEVEL_NUM-1 then
                        if is_serve_A_arr(i+1) = '1' then
                          if current_fifo_byte_cnt_A(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                            lvl_deq_fifo_index <= current_fifo_arr(i+1);
                            lvl_deq_cmd_A(i+1) <= '1';
                            v_deq_level := i + 1;
                            vc <= vc + 1;
                          else
                            lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                            lvl_deq_cmd_A(i) <= '1';
                            v_deq_level := i;
                            vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                            if i > 0 then
                              vc(i*g_L2_FIFO_NUM - 1 downto 0) <= (others => '0');
                            end if;
                          end if;
                        else -- B set active in next level
                          if i < g_L2_LEVEL_NUM - 2 then
                            if current_fifo_byte_cnt_B(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                              lvl_deq_fifo_index <= current_fifo_arr(i+1);
                              lvl_deq_cmd_B(i+1) <= '1';
                              v_deq_level := i + 1;
                              vc <= vc + 1;
                            else
                              lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                              lvl_deq_cmd_A(i) <= '1';
                              v_deq_level := i;
                              vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                              if i > 0 then
                                vc(i*g_L2_FIFO_NUM - 1 downto 0) <= (others => '0');
                              end if;
                            end if;
                          end if;
                        end if;
                      else -- i = g_LEVEL_NUM-1 (highest level)
                        lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                        lvl_deq_cmd_A(i) <= '1';
                        v_deq_level := i;
                        vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                        vc(i*g_L2_FIFO_NUM - 1 downto 0) <= (others => '0');
                      end if;
                    else -- earliest fifo is current fifo + 1
                      lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                      lvl_deq_cmd_A(i) <= '1';
                      v_deq_level := i;
                      vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                      if i > 0 then
                        vc(i*g_L2_FIFO_NUM-1 downto 0) <= (others => '0');
                      end if;
                    end if;  
                       
                    bytes_to_serve(v_deq_level) <= resize(signed(earliest_fifo_byte_cnt_A(v_deq_level)(g_BYTE_CNT_WIDTH - 1 downto v_deq_level*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                    v_vc_updated := true;
                   if i > 0 and i < g_LEVEL_NUM - 1 then
                     is_serve_A_arr(i-1) <= not earliest_fifo_index_A(i)(0);
                   end if;
                 else -- vc was updated in one of previous loops
                   bytes_to_serve(i) <= resize(signed(current_fifo_byte_cnt_A(i)(g_BYTE_CNT_WIDTH - 1 downto i*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                 end if;
               else -- all fifos in set A are empty
                 if i < g_LEVEL_NUM-2 then
                   if all_fifos_empty_B(i) = '0' then
                     -- update VC slice corresponding to level
                     if not v_vc_updated then
                       if earliest_fifo_index_B(i) > current_fifo_arr(i) + 1 then
                         if i < g_LEVEL_NUM-1 then
                           if is_serve_A_arr(i+1) = '1' then
                              if current_fifo_byte_cnt_A(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                                lvl_deq_fifo_index <= current_fifo_arr(i+1);
                                lvl_deq_cmd_A(i+1) <= '1';
                                v_deq_level := i + 1;
                                vc <= vc + 1;
                              else
                                lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                                lvl_deq_cmd_A(i) <= '1';
                                v_deq_level := i;
                                vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                              end if;
                            else -- B set active in next level
                              if current_fifo_byte_cnt_B(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                                lvl_deq_fifo_index <= current_fifo_arr(i+1);
                                lvl_deq_cmd_B(i+1) <= '1';
                                v_deq_level := i + 1;
                                vc <= vc + 1;
                              else
                                lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                                lvl_deq_cmd_A(i) <= '1';
                                v_deq_level := i;
                                vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                              end if;
                            end if;
                          else -- i = g_LEVEL_NUM-1 (highest level)
                            lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                            lvl_deq_cmd_A(i) <= '1';
                            v_deq_level := i;
                            vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                            vc(i*g_L2_FIFO_NUM - 1 downto 0) <= (others => '0');
                          end if;
                        else -- earliest fifo is current fifo + 1
                          lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                          lvl_deq_cmd_A(i) <= '1';
                          v_deq_level := i;
                          vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                        end if;  
                        -- calculate number of bytes to serve according to dequeue level
                        bytes_to_serve(v_deq_level) <= resize(signed(earliest_fifo_byte_cnt_A(v_deq_level)(g_BYTE_CNT_WIDTH - 1 downto v_deq_level*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                        v_vc_updated := true;
                        if i > 0 and i < g_LEVEL_NUM - 1 then
                          is_serve_A_arr(i-1) <= not earliest_fifo_index_A(i)(0);
                        end if;
                      else
                        bytes_to_serve(i) <= resize(signed(current_fifo_byte_cnt_B(i)(g_BYTE_CNT_WIDTH - 1 downto i*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                      end if;
                    end if; 
                  end if;
                end if;
              else -- set B
                if i < g_LEVEL_NUM-2 then
                  if all_fifos_empty_B(i) = '0' then
                    -- update VC slice corresponding to level
                    if not v_vc_updated then
                      --if i < g_LEVEL_NUM-1 then
                      if earliest_fifo_index_B(i) > current_fifo_arr(i) + 1 then
                        if is_serve_A_arr(i+1) = '1' then
                          if current_fifo_byte_cnt_A(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                            lvl_deq_fifo_index <= current_fifo_arr(i+1);
                            lvl_deq_cmd_A(i+1) <= '1';
                            v_deq_level := i + 1;
                            vc <= vc + 1;
                          else
                            lvl_deq_fifo_index <= earliest_fifo_index_B(i);
                            lvl_deq_cmd_B(i) <= '1';
                            v_deq_level := i;
                            vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_B(i);
                          end if;
                        else -- B set active in next level
                          if current_fifo_byte_cnt_B(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                            lvl_deq_fifo_index <= current_fifo_arr(i+1);
                            lvl_deq_cmd_B(i+1) <= '1';
                            v_deq_level := i + 1;
                            vc <= vc + 1;
                          else
                            lvl_deq_fifo_index <= earliest_fifo_index_B(i);
                            lvl_deq_cmd_B(i) <= '1';
                            v_deq_level := i;
                            vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_B(i);
                          end if;
                        end if;
                      else -- earliest fifo is current fifo + 1
                        lvl_deq_fifo_index <= earliest_fifo_index_B(i);
                        lvl_deq_cmd_B(i) <= '1';
                        v_deq_level := i;
                        vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_B(i);
                      end if;
                      bytes_to_serve(v_deq_level) <= resize(signed(earliest_fifo_byte_cnt_B(v_deq_level)(g_BYTE_CNT_WIDTH - 1 downto v_deq_level*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                      v_vc_updated := true;
                      if i > 0 and i < g_LEVEL_NUM - 1 then
                        is_serve_A_arr(i-1) <= not earliest_fifo_index_A(i)(0);
                      end if;
                    else
                      bytes_to_serve(i) <= resize(signed(current_fifo_byte_cnt_B(i)(g_BYTE_CNT_WIDTH - 1 downto i*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                    end if;
                  elsif all_fifos_empty_A(i) = '0' then
                    -- update VC slice corresponding to level
                    if not v_vc_updated then
                      if earliest_fifo_index_A(i) > current_fifo_arr(i) + 1 then
                        if is_serve_A_arr(i+1) = '1' then
                          if current_fifo_byte_cnt_A(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                            lvl_deq_fifo_index <= current_fifo_arr(i+1);
                            lvl_deq_cmd_A(i+1) <= '1';
                            v_deq_level := i + 1;
                            vc <= vc + 1;
                          else
                            lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                            lvl_deq_cmd_A(i) <= '1';
                            v_deq_level := i;
                            vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                          end if;
                        else -- B set active in next level
                          if current_fifo_byte_cnt_B(i+1) /= to_unsigned(0, g_BYTE_CNT_WIDTH) then
                            lvl_deq_fifo_index <= current_fifo_arr(i+1);
                            lvl_deq_cmd_B(i+1) <= '1';
                            v_deq_level := i + 1;
                            vc <= vc + 1;
                          else
                           lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                           lvl_deq_cmd_A(i) <= '1';
                           v_deq_level := i;
                           vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                          end if;
                        end if;
                      else -- earliest fifo is current fifo + 1
                        lvl_deq_fifo_index <= earliest_fifo_index_A(i);
                        lvl_deq_cmd_A(i) <= '1';
                        v_deq_level := i;
                        vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM) <= earliest_fifo_index_A(i);
                      end if;  
                       
                      bytes_to_serve(v_deq_level) <= resize(signed(earliest_fifo_byte_cnt_A(v_deq_level)(g_BYTE_CNT_WIDTH - 1 downto v_deq_level*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                      v_vc_updated := true;
                      if i > 0 then
                        is_serve_A_arr(i-1) <= not earliest_fifo_index_A(i)(0);
                      end if;
                    else
                      bytes_to_serve(i) <= resize(signed(current_fifo_byte_cnt_A(i)(g_BYTE_CNT_WIDTH - 1 downto i*g_L2_FIFO_NUM)), g_BYTE_CNT_WIDTH);
                    end if;
                  end if;
                end if;
              end if;
            end if;
          end loop;
          if find_earliest_non_empty_fifo_rsp_A(0) = '1' then
            if and_reduce(all_fifos_empty_A) = '0' or and_reduce(all_fifos_empty_B) = '0' then
              deqd_level <= to_unsigned(v_deq_level, g_LEVEL_NUM);
              deq_state <= WAIT_DEQ_VLD;
            else
              -- !!! output deq_empty?
              deq_state <= IDLE;
            end if;
          end if;
                    
        when SERVE_BYTES =>
          v_not_served := (others => '1');
          if (deq_cmd = '1') then
            for i in 0 to g_LEVEL_NUM-1 loop
              if (bytes_to_serve(i) > 0) and (and_reduce(v_not_served(i downto 0)) = '1') then
                if is_serve_A_arr(i) = '1' then
                  lvl_deq_cmd_A(i) <= '1';
                else
                  if i < g_LEVEL_NUM-1 then
                    lvl_deq_cmd_B(i) <= '1';
                  end if;
                end if;
                lvl_deq_fifo_index <= vc((i+1)*g_L2_FIFO_NUM-1 downto i*g_L2_FIFO_NUM);
                deq_state <= WAIT_DEQ_VLD;
                v_not_served(i) := '0';
                deqd_level <= to_unsigned(i, g_LEVEL_NUM);
              end if;
            end loop;
            if and_reduce(v_not_served) = '1' then
              deq_state <= IDLE;
            end if;
          end if;
          
        when WAIT_DEQ_VLD =>
          if lvl_deq_desc_valid_A(to_integer(deqd_level)) = '1' then
            -- update served bytes of the dequed level
            v_deqd_bytes := unsigned(lvl_deq_desc_A(to_integer(deqd_level))(g_DESC_BIT_WIDTH-1 downto g_DESC_BIT_WIDTH-PKT_LEN_BIT_WIDTH));
            bytes_to_serve(to_integer(deqd_level)) <= bytes_to_serve(to_integer(deqd_level)) - resize(signed(v_deqd_bytes), g_BYTE_CNT_WIDTH);
            gb_deq_pkt_cnt <= gb_deq_pkt_cnt + 1;
          end if;
          if lvl_deq_desc_valid_B(to_integer(deqd_level)) = '1' then
            v_deqd_bytes := unsigned(lvl_deq_desc_B(to_integer(deqd_level))(g_DESC_BIT_WIDTH-1 downto g_DESC_BIT_WIDTH-PKT_LEN_BIT_WIDTH));
            bytes_to_serve(to_integer(deqd_level)) <= bytes_to_serve(to_integer(deqd_level)) - resize(signed(v_deqd_bytes), g_BYTE_CNT_WIDTH);
            -- update pkt_cnt of gearbox scheduler
            gb_deq_pkt_cnt <= gb_deq_pkt_cnt + 1;
          end if;
          if lvl_deq_desc_valid_A(to_integer(deqd_level)) = '1' or lvl_deq_desc_valid_B(to_integer(deqd_level)) = '1' then
            v_more_bytes_to_serve := false;
            if (bytes_to_serve(to_integer(deqd_level)) - resize(signed(v_deqd_bytes), g_BYTE_CNT_WIDTH) > 0) then
                deq_state <= SERVE_BYTES;
                v_more_bytes_to_serve := true;
            else
              for i in 0 to g_LEVEL_NUM-1 loop
                if i /= to_integer(deqd_level) then
                  if bytes_to_serve(i) > 0 then
                    deq_state <= SERVE_BYTES;
                    v_more_bytes_to_serve := true;
                  end if;
                end if;
              end loop;
            end if;
            if not v_more_bytes_to_serve then
--              if deq_cmd = '1' then
--                deq_state <= WAIT_1ST_NON_EMPTY;
--              else
                deq_state <= IDLE;
--              end if;
            end if;
          end if;
        end case;
    end if; 
  end process p_dequeue;

  deq_desc <= lvl_deq_desc_A(to_integer(deqd_level)) when lvl_deq_desc_valid_A(to_integer(deqd_level)) = '1' else
              lvl_deq_desc_B(to_integer(deqd_level)) when lvl_deq_desc_valid_B(to_integer(deqd_level)) = '1' else
              (others => 'X');
  deq_desc_valid <= lvl_deq_desc_valid_A(to_integer(deqd_level)) or lvl_deq_desc_valid_B(to_integer(deqd_level));
  
  gb_pkt_cnt <= gb_enq_pkt_cnt - gb_deq_pkt_cnt;  

end gearbox_I_arch;