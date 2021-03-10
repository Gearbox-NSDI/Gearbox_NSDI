-- Code your testbench here
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.gb_package.all;

entity gearboxI_tb is
end gearboxI_tb;

architecture tb of gearboxI_tb is
component gearbox_I
  generic (
    g_L2_FLOW_NUM     : integer;     -- log2 of number of flows    g_LEVEL_NUM       : integer;     -- number of levels
    g_L2_LEVEL_NUM    : integer;     -- log2 of number of levels
    g_L2_FIFO_NUM     : integer;     -- log2 of number of FIFOs
    g_L2_FIFO_SIZE    : integer;     -- log2 of size (depth) of each FIFO
    g_DESC_BIT_WIDTH  : integer;     -- Descriptor width
    g_VC_BIT_WIDTH    : integer;     -- number of bits in VC
    g_PKT_CNT_WIDTH   : integer;     -- packet count width
    g_BYTE_CNT_WIDTH  : integer      -- byte count width
  );
  port (
    rst                              : in  std_logic;
    clk                              : in  std_logic;
    -- enq i/f
    enq_cmd                          : in  std_logic;
    enq_desc                         : in  std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    enq_done                         : out std_logic;
    -- deq i/f
    deq_cmd                          : in  std_logic;
    deq_desc                         : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    deq_desc_valid                   : out std_logic;
    -- drop i/f
    drop_cmd                         : out std_logic;
    drop_desc                        : out std_logic_vector(DESC_BIT_WIDTH-1 downto 0);
    -- pkt count i/f
    gb_pkt_cnt                       : out unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0)

  );
end component;

constant  g_L2_FLOW_NUM                    : integer := 10;
constant  g_L2_FIFO_NUM                    : integer := 4;
constant  g_L2_LEVEL_NUM                   : integer := 2;
constant  g_LEVEL_NUM                      : integer := 2**g_L2_LEVEL_NUM;
constant  g_L2_FIFO_SIZE                   : integer := 8;
constant  g_DESC_BIT_WIDTH                 : integer := DESC_BIT_WIDTH; -- from gb_package
constant  g_VC_BIT_WIDTH                   : integer := 20;
constant  g_PKT_CNT_WIDTH                  : integer := 9;
constant  g_BYTE_CNT_WIDTH                 : integer := 11+9;

constant  MAX_SIM_TIME : time    := 500 ns;
constant  CLK_PERIOD   : time    := 3 ns;
constant  LOGIC_DELAY  : time    := 100 ps;
constant  MAX_DEQ_TIME : integer := 10;

procedure proc_wait_deq_vld (
  signal clk: in std_logic;
  signal deq_desc_valid: in std_logic;
  constant MAX_DEQ_TIME: in integer range 0 to MAX_DEQ_TIME-1
  ) is
  variable i : integer := 0;
  begin
    i := 0;
    while deq_desc_valid /= '1' and i < MAX_DEQ_TIME loop
      wait until rising_edge(clk);
      i := i + 1;
    end loop;
    assert (deq_desc_valid = '1') report "Dequeue time out" severity failure;
  end procedure;
   
signal    rst                              : std_logic := '0';
signal    clk                              : std_logic := '0';
    -- enq i/f
signal    enq_cmd                          : std_logic := '0';
signal    enq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    enq_done                         : std_logic := '0';
    -- deq i/f
signal    deq_cmd                          : std_logic := '0';
signal    deq_desc                         : std_logic_vector(g_DESC_BIT_WIDTH-1 downto 0) := (others => '0');
signal    deq_desc_valid                   : std_logic := '0';
    -- drop i/f
signal    drop_cmd                         : std_logic := '0';
signal    drop_desc                        : std_logic_vector(DESC_BIT_WIDTH-1 downto 0) := (others => '0');
    -- pkt count i/f
signal    gb_pkt_cnt                       : unsigned(g_L2_LEVEL_NUM+g_L2_FIFO_NUM+g_BYTE_CNT_WIDTH-1 downto 0) := (others => '0');

begin

i_gb: gearbox_I
  generic map(
    g_L2_FLOW_NUM     => g_L2_FLOW_NUM,    -- log2 of number of flows
    g_L2_LEVEL_NUM    => g_L2_LEVEL_NUM,   -- log2 of number of levels
    g_L2_FIFO_NUM     => g_L2_FIFO_NUM,    -- log2 of number of FIFOs
    g_L2_FIFO_SIZE    => g_L2_FIFO_SIZE,   -- log2 of size (depth) of each FIFO
    g_DESC_BIT_WIDTH  => g_DESC_BIT_WIDTH, -- Descriptor width
    g_VC_BIT_WIDTH    => g_VC_BIT_WIDTH,   -- number of bits in VC
    g_PKT_CNT_WIDTH   => g_PKT_CNT_WIDTH,  -- packet count width
    g_BYTE_CNT_WIDTH  => g_BYTE_CNT_WIDTH  -- byte count width
  )
  port map (
    rst                              => rst,
    clk                              => clk,
    -- enq i/f
    enq_cmd                          => enq_cmd,
    enq_desc                         => enq_desc,
    enq_done                         => enq_done,
    -- deq i/f
    deq_cmd                          => deq_cmd,
    deq_desc                         => deq_desc,
    deq_desc_valid                   => deq_desc_valid,
    -- drop i/f
    drop_cmd                         => drop_cmd,
    drop_desc                        => drop_desc,
    -- pkt count i/f
    gb_pkt_cnt                       => gb_pkt_cnt
  );
  

p_clk: process
  begin
    while NOW < MAX_SIM_TIME loop
      clk <= not clk;
      wait for CLK_PERIOD/2;
    end loop;
    wait;
end process p_clk;

p_main: process
alias gb_lvl_enq_cmd_A is << signal i_gb.lvl_enq_cmd_A : std_logic_vector(g_LEVEL_NUM-1 downto 0) >> ;     -- enq level index set A
alias gb_lvl_enq_cmd_B is << signal i_gb.lvl_enq_cmd_B : std_logic_vector(g_LEVEL_NUM-2 downto 0) >> ;     -- enq level index set B
alias gb_lvl_enq_fifo_index is << signal i_gb.lvl_enq_fifo_index : unsigned(g_L2_FIFO_NUM-1 downto 0) >> ; -- enq fifo index
alias gb_fin_time is << signal i_gb.fin_time : unsigned(FIN_TIME_BIT_WIDTH-1 downto 0) >> ;    -- fin time of current pkt
alias gb_enq_level is << signal i_gb.enq_level : unsigned(g_L2_LEVEL_NUM-1 downto 0) >> ;    -- enq_level of current pkt

  begin

    wait until rising_edge(clk);
    rst <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);
    rst <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);

    -- enque testing

    -- Enq 01-------------------------------------------------
    -- 1. enqueue a desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*5, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(5, FIN_TIME_BIT_WIDTH))   &  -- TODO: pkt transmission time = 5
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        &  -- TODO: flow id = 0
                                        std_logic_vector(to_unsigned(0, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 5
    assert (to_integer(gb_fin_time) = 5) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;

    -- Enq 02-------------------------------------------------
    -- 2. enqueue a subsequent desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*12, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(12, FIN_TIME_BIT_WIDTH))  &  -- TODO: pkt transmission time = 12
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        &  -- TODO: flow id = 0
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
      
    wait until rising_edge(clk);
    -- @ (after) clk 03: check Enq 1
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0001") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 5
    assert (to_integer(gb_lvl_enq_fifo_index) = 5) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;
    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;
    
    --  falling edge of Enq 2;                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 17
    assert (to_integer(gb_fin_time) = 17) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 1
    assert (to_integer(gb_enq_level) = 1) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
                    
    -- Enq 03-------------------------------------------------
    -- 3. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*6, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(6, FIN_TIME_BIT_WIDTH))  &  -- TODO: pkt transmission time = 6
                                        std_logic_vector(to_unsigned(1, g_L2_FLOW_NUM))        &  -- TODO: flow id = 1
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;

    wait until rising_edge(clk);

    -- @ (after) clk 03: check Enq 2
    -- Check final enque level (v_enq_level)
    -- Final enque level = 1
    --assert (to_integer(gb_v_enq_level) = 1) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;    

 --   wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);

    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 6
    assert (to_integer(gb_fin_time) = 6) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
 
     -- Enq 04-------------------------------------------------
    -- 4. enqueue a desc into Gearbox with flow_id = 2
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(99999, FIN_TIME_BIT_WIDTH))  &  -- TODO: pkt transmission time = 9999
                                        std_logic_vector(to_unsigned(2, g_L2_FLOW_NUM))        &  -- TODO: flow id = 2
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                   
    wait until rising_edge(clk);
    -- @ (after) clk 03: Check Enq 3
    -- Check final enque level (v_enq_level)
    -- Final enque level = 1
    --assert (to_integer(gb_v_enq_level) = 1) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0001") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 6) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

--    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);

    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 9999
    assert (to_integer(gb_fin_time) = 99999) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '1') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;

    -- Deq 01-------------------------------------------------
    wait until rising_edge(clk);

    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);
    --wait until rising_edge(clk);
    --wait until rising_edge(clk);
    -- Deq 02-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    --wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);
    --wait until rising_edge(clk);
    -- Deq 03-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);

    -- Enq 04-------------------------------------------------

    --@VC = 16, Enq: [fid = 0, size = 64*2, trans_time = 2, pkt_id = 2]

    --clk 01:
      -- fin_time = 19
    --clk 02:
      -- drop_cmd = 0
      -- enq_level = 0
    --clk 03:
      -- lvl_enq_cmd_A = "0010" # since last enq level of this fid = 1, enque level 1 set A
      -- lvl_enq_cmd_B = "000"
      -- lvl_enq_fifo_index = 1

    -- pkt_cnt = 1

    -- 1. enqueue a desc into Gearbox with flow_id = 0
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*2, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(2, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(2, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check fin_time:
    -- Fin time = 19
    assert (to_integer(gb_fin_time) = 19) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

    -- Enq 05-------------------------------------------------

    --@VC = 16, Enq: [fid = 1, size = 64*2, trans_time = 2, pkt_id = 1]

    --clk 01:
    -- fin_time = 18
    --clk 02:
    -- drop_cmd = 0
    -- enq_level = 0
    --clk 03:
    -- lvl_enq_cmd_A = "0000"
    -- lvl_enq_cmd_B = "001"
    -- lvl_enq_fifo_index = 2

    -- pkt_cnt = 2

    -- 1. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*2, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(2, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(1, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(1, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 18
    assert (to_integer(gb_fin_time) = 18) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0000") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "001") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 2
    assert (to_integer(gb_lvl_enq_fifo_index) = 2) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

    -- Enq 06-------------------------------------------------

    --@VC = 16, Enq: [fid = 0, size = 64*2, trans_time = 2, pkt_id = 3]

  --clk 01:
    -- fin_time = 21
  --clk 02:
    -- drop_cmd = 0
    -- enq_level = 0
  --clk 03:
    -- lvl_enq_cmd_A = "0010" # since last enq level of this fid = 1, enque level 1 set A
    -- lvl_enq_cmd_B = "000"
    -- lvl_enq_fifo_index = 1

  -- pkt_cnt = 3

    -- 1. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*2, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(2, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(3, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 21
    assert (to_integer(gb_fin_time) = 21) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure

    -- Enq 07-------------------------------------------------

    --@VC = 16, Enq: [fid = 0, size = 64*2, trans_time = 2, pkt_id = 4]

  --clk 01:
    -- fin_time = 23
  --clk 02:
    -- drop_cmd = 0
    -- enq_level = 0
  --clk 03:
    -- lvl_enq_cmd_A = "0010" # since last enq level of this fid = 1, enque level 1 set A
    -- lvl_enq_cmd_B = "000"
    -- lvl_enq_fifo_index = 1

  -- pkt_cnt = 4

    -- 1. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*2, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(2, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(4, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 23
    assert (to_integer(gb_fin_time) = 23) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

    -- Enq 08-------------------------------------------------

    --@VC = 16, Enq: [fid = 0, size = 64*2, trans_time = 2, pkt_id = 5]

  --clk 01:
    -- fin_time = 25
  --clk 02:
    -- drop_cmd = 0
    -- enq_level = 0
  --clk 03:
    -- lvl_enq_cmd_A = "0010" # since last enq level of this fid = 1, enque level 1 set A
    -- lvl_enq_cmd_B = "000"
    -- lvl_enq_fifo_index = 1

  -- pkt_cnt = 5

    -- 1. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*2, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(2, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(0, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(5, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 25
    assert (to_integer(gb_fin_time) = 25) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0010") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "000") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 1
    assert (to_integer(gb_lvl_enq_fifo_index) = 1) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;

    -- Enq 09-------------------------------------------------

    --@VC = 16, Enq: [fid = 1, size = 64*12, trans_time = 12, pkt_id = 1]

  --clk 01:
    -- fin_time = 30
  --clk 02:
    -- drop_cmd = 0
    -- enq_level = 0
  --clk 03:
    -- lvl_enq_cmd_A = "0000" # since last enq level of this fid = 1, enque level 1 set A
    -- lvl_enq_cmd_B = "001"
    -- lvl_enq_fifo_index = 14

  -- pkt_cnt = 5

    -- 1. enqueue a desc into Gearbox with flow_id = 1
    enq_cmd                          <= '1' after LOGIC_DELAY;
    enq_desc                         <= std_logic_vector(to_unsigned(64*12, PKT_LEN_BIT_WIDTH))   &
                                        std_logic_vector(to_unsigned(12, FIN_TIME_BIT_WIDTH))   &  
                                        std_logic_vector(to_unsigned(1, g_L2_FLOW_NUM))        & 
                                        std_logic_vector(to_unsigned(2, PKT_ID_BIT_WIDTH)) after LOGIC_DELAY;
                                        
    wait until rising_edge(clk);                
    enq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    -- @ (after) clk 01:
    -- Check fin_time:
    -- Fin time = 30
    assert (to_integer(gb_fin_time) = 30) report "gb_fin_time = " & INTEGER'IMAGE(to_integer(gb_fin_time)) severity failure;                

    wait until rising_edge(clk);
    -- @ (after) clk 02:
    -- Check last fin time [ToDo]
    -- Check last enq level [ToDo]
    -- Check drop_cmd:
    -- Drop cmd = false (0)
    assert (drop_cmd = '0') report "drop_cmd = " & STD_LOGIC'IMAGE(drop_cmd) severity failure;  
    
    -- Check enque level
    -- Enque level = 0
    assert (to_integer(gb_enq_level) = 0) report "gb_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_enq_level))) severity failure;
    
    
    wait until rising_edge(clk);
    -- @ (after) clk 03:
    -- Check final enque level (v_enq_level)
    -- Final enque level = 0
    --assert (to_integer(gb_v_enq_level) = 0) report "gb_v_enq_level = " & INTEGER'IMAGE(to_integer(unsigned(gb_v_enq_level))) severity failure;
    -- Check enque set
    -- Enque set A
    assert (gb_lvl_enq_cmd_A = "0000") report "gb_lvl_enq_cmd_A = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_A))) severity failure;
    assert (gb_lvl_enq_cmd_B = "001") report "gb_lvl_enq_cmd_B = " & INTEGER'IMAGE(to_integer(unsigned(gb_lvl_enq_cmd_B))) severity failure;
    -- Check enque fifo index
    -- Enque fifo 14
    assert (to_integer(gb_lvl_enq_fifo_index) = 14) report "gb_lvl_enq_fifo_index = " & INTEGER'IMAGE(to_integer(gb_lvl_enq_fifo_index)) severity failure;

    --assert (enq_done = '1') report "enq_done = " & STD_LOGIC'IMAGE(enq_done) severity failure;
 
    -- Deq 05-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);
    
    -- Deq 06-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);

    -- Deq 07-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);

    -- Deq 08-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);

    -- Deq 09-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);

    -- Deq 10-------------------------------------------------
    deq_cmd                          <= '1' after LOGIC_DELAY;
    wait until rising_edge(clk);                
    deq_cmd                          <= '0' after LOGIC_DELAY;
    wait until rising_edge(clk);
    proc_wait_deq_vld(clk, deq_desc_valid, MAX_DEQ_TIME);
   
    wait;
end process p_main;
    
 

end tb;