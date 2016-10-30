source("functions.R")

#path <- "/home/asmundb/SURFEX2/open_SURFEX_V8_0/MY_RUN/KTEST/1D_test_case/Results/open_loop/"
path <- "/home/asmundb/SURFEX2/open_SURFEX_V8_0/MY_RUN/KTEST/1D_test_case/Results/analysis/"

#path <- "/home/asmundb/SURFEX2/SODA_v8_tb/MY_RUN/KTEST/1D_test_case/Results/open_loop/"
path <- "/home/asmundb/SURFEX2/SODA_v8_tb/MY_RUN/KTEST/1D_test_case/Results/analysis/"

files <- list.files(path, pattern="WG1.*", full.names=T)



