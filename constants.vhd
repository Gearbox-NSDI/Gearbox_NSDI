package gb_package is 
  constant PKT_LEN_BIT_WIDTH  : integer := 11;
  constant FIN_TIME_BIT_WIDTH : integer := 20;
  constant FLOW_ID_BIT_WIDTH  : integer := 10;
  constant PKT_ID_BIT_WIDTH   : integer := 16;
  constant DESC_BIT_WIDTH     : integer := (PKT_LEN_BIT_WIDTH + FIN_TIME_BIT_WIDTH + FLOW_ID_BIT_WIDTH + PKT_ID_BIT_WIDTH);
end package gb_package;