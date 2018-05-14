.
├── Src                    
│   ├── a4.hs  # source code
│   └── a4     # executable file
│
├── Example
│   ├── JSON   # folder contains some test json files
│   │   ├── 1.json
│   │   ├── 2.json 	
│   │   └── ...
│   │	
│   └── HTML   # folder contains the corresponding result html files
│       ├── 1.html
│       ├── 2.html 	
│       └── ...
│                    
└── ReadMe.txt


Compiling Command:  
ghc a4.hs


Running Command:
./a4 ${inputFilePath} > ${outputFilePath}