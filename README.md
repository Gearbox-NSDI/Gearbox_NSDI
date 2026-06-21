## IEEE/ACM Transactions on Networking journal extension
Release: Gearbox-ToN-2026
This release provides the implementation artifact associated with:

"Gearbox: A Hierarchical FIFO-Based Scheduler Approximating
Weighted Fair Queuing"

This repository originated with the NSDI 2022 Gearbox paper.
The present release identifies the source and implementation
materials associated with the IEEE/ACM Transactions on
Networking journal extension.

Contents include:
- Gearbox VHDL source
- VHDL testbench
- FPGA implementation and utilization materials
  
# Gearbox_NSDI
GitHub repository for NSDI paper: Gearbox: A Hierarchical Packet Scheduler for ApproximateWeighted Fair Queuing


Gearbox_Xilinx: 		Implementation report of Gearbox hardware prototype on a Xilinx Alveo U250 FPGA card 

gearbox_I.vhd: 			Top design of Gearbox

gearbox_level.vhd: 		Level module of Gearbox

last_enq_level_arr.vhd:	Memory to store "last enqueue level" for each flow

fin_time_arr.vhd:		Memory to store "last packet finish time" for each flow

constants.vhd:			Design constants for sizing various parameters

tb_gearboxI_test_set_01_0219.vhd : 	Testbench for Gearbox
