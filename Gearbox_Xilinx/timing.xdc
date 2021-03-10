create_clock -period 2.85 -name clk -waveform {0.000 1.667} [get_ports -filter { NAME =~  "*clk*" && DIRECTION == "IN" }]
