#!/bin/bash 

echo "source Vivado settings.sh before running" 

export SDK_AVAILABLE=1
export VIVADO_VERSION=2015.4
export ZIP_PATH=zip
export PARTNUMBER=3

# todo: check if vlog exists or create it

if [ ! -d "v_log" ]; then
  mkdir v_log
fi

cd v_log 

echo "Programming FPGA" 
vivado -source ../scripts/script_main.tcl  -mode batch -notrace -tclargs --program_bit --boardpart $PARTNUMBER

echo "Flashing software" 
vivado -source ../scripts/script_main.tcl  -mode batch -notrace -tclargs --program_bin --boardpart $PARTNUMBER --program_swapp display_test

