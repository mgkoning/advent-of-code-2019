# advent-of-code-2019
My solutions to Advent of Code 2019

## Running Haskell solutions
The Haskell project is defined as a Haskell Stack package.

To install Stack:\
https://docs.haskellstack.org/en/stable/README/#how-to-install

After that, enter `stack run` on the command line in 
the `advent-of-haskell` directory.

By default, the runner determines the current day of month and runs the puzzle
solution for that day if available. Otherwise, you can specify the day to run
using `stack run -- 1` (to run day 1).

## Running .NET Core solutions
Install the .NET Core 3.0+ SDK:\
https://dotnet.microsoft.com/download

Enter `dotnet run -p advent-of-net-core/` in the `advent-of-net-core`
directory.

By default, the runner determines the current day of month and runs the puzzle
solution for that day if available. Otherwise, you can specify the day to run
using `dotnet run -p advent-of-net-core/ -- 1` (to run day 1).

## It's like art (day 11)
```
         ## ##
        #    #
       # # ##                      ##
       ##  ## ##                  # #
       ###  # #  #                # #
      ######  ####                # #
     #   #####     #  ###        ##
      ####   # #   #    ###     ####
 #### # ## #   #  ## ## ###          ##
 #  ## #### #   #   #  ##   ##   #    #
 #    # ##  # ### ### ### ### ##    #
  # # ## ####   ##  # ## # #  # #  ##  #
 #   # ## # #   # # #  ##  #  # ##  # ##
 #    #  #####   #  # # # # ######  # #
 #  #   ##    #   # ### # #     ### ##
          ####   #####  #   #  ##  #
  ####    # #  ##      # ## #  ####
   ##     ##   #   ##  ##  ### #  ## ###
 # #       ### ## ## #    ##   # ##    #
 # #   ## # #  # #  ## ##  ##   # ##  #
 # #   # ####  ##  ## #  #   ###   #  #
 ##   ##  # ## # # ##  #  ## ##   # ###
     #      # # ##     #   ####  # #
     # #   ## ## # #  ##         ##
      ### #  ## # ## # # #    #   #
       #  # # ##   #  ##       ####
         #   ###   ##    ####   ##
        #   ### ###### #    #
       #    # #  ### ## ####
     ### ###    ##   # #  ##                    ##
       #    #### # #   # ####        ##         # ###
       ### ### # #  # #  # ###       #  ###     #   #
    # # #  ##    ####  #  #   #        #  #  #      #
    ## ##  ### ##    # #    ###       # #   ##  # ## #
      #     # #       #  # ###      # ## ## #   # # ##
      ##      # ## #####            ### #       # #
              ##     #### #  #        ######     ##
                       #  #             ## #  # #   #
                      # # ###  #     # ###  ## #  ## #     ##
                    #  ##     #      #   # # ## ## # # ##   #
                    ##    ##  # #    #     ## ######## #   ##
                     #  #  ##  ## #######  ####   #  ## #  #
                     ##  ### #   #### ##  # ##  ###    ######
                    # #  ####   #######   ##  # # # ### ###
                   #  # ## # ###    ## ####  #  #   ####
                   #   #  ###   #####    # # ## #  # # # #
                     ## # ## ###    ###   #### # ### ##   #
                    # #   ## ## ##     #####   #     ## ##
                          ### #  ### ##  ##     # ##  # #
                          # ##  # ##    #   ##    ##     ###
                                  ## #     # # #   #  #####
                            ##     #  # ##  # # ###   #   # #
                            ##  # # #       #   # ####   # # #
                             ## ## ##  # #   ## ##  ### # ####
                              ### ###### #   ## ## # ## #### #
                               ## ## #  ## #   #  #######  ##
                         ##    ##     #### ##    ##   ###  ## #
                         # #       ####  ##    #    ### ## ####
                          # ## ##  #    ##    # ####   ##  # ##
                          # ##      ## ## ## #   ##  # ##  ## #
                         #  #####  #### # ###     ## ## ##### #
                         #  # #  ## ###  ##      #### ## ##  ##
                          #  #    #  #     # ##  #### # ##  ## ##
                           ###  #   ##     #####  ## # ##     ##
                            #  ## #        ###     ####  #     # #
                             #  ###       ####   ## #   #   # ### ##
                                          ###  ###   ###   ##  # # #
                                          ##        # # # ###  ####
                                         ##    #   # ### #     #  #
                                        ### #    ######   ##### ###
                                       # # ## #    #    #  #  ##
                                       #   ## ## ####   ## #####
                                        #   ## ##  ##  ####     ###
                                         ###   ### #### # ######  #
                                         #  #  ###  ## #        ##
                                        ### #  # ## # #   #
                                          # #####  #  ##  #
                                     ####  #  # #  #   ##
                                      ##    # # ###  #
                                     ##     ##    # #
                                      # #  ####   ##
                                        ##
                                        ####
                                        # #
```