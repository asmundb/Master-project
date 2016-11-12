#### Master-project ####


This repository contains scripts mainly for data handling and visualization
related to my master study. Scripts for running SURFEX should also be added including namelist etc.


* Makefile: 
  automates plotting 

  Dependencies

  NC\_FILES(SURFEX output)    read\_nc.R    plot\_timeserie.R
                   \            /            /
				    \          /            /
					 \        /            /
					  \      /            /
					   \    /            /
		               *.dat            /
		                   \           /
						    \         /
							 \       /
							  \     /
							   *.pdf



ToDoList:
 * SURFEX
   - establish openloop
   - set up EKFrun
   - set up EnKFrun
