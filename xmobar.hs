Config { font = "xft:IBM Plex Mono Medium"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = Top
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run DynNetwork ["-L","0","-H","32",
       					"--low","green","--normal","orange","--high","red"
					] 10
                    , Run MultiCpu ["-t","Cpu: <total>%","-L","3","-H","50",
		    			"--low","green","--normal","orange","--high","red"
					] 10
                    , Run Memory ["-t","Mem: <usedratio>%",
		    			"--low","green","--normal","orange","--high","red"
		    			] 10
                    , Run Swap ["-t","Swap: <usedratio>%",
		    			"--low","green","--normal","orange","--high","red"
		    			] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
		    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% | %memory% | %swap% | %dynnetwork% | <fc=#ee9a00>%date%</fc>"
       }
