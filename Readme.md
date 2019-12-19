# Repliss

The **repl**icated **i**nformation **s**ystem verification tool for the development of applications with *s*trong guarantees on weakly consistent data stores.

# Getting started

See [User Documentation](./documentation/doc.md). 

To use Repliss there are several options:

1. Use the hosted version at https://softech.cs.uni-kl.de/repliss/
2. Run the web interface using Docker:
    
    
3.



# Compilation

Software Requirements:

- The [Scala Build Tool (SBT)](http://www.scala-sbt.org/)
- Operation system: x86_64-linux (for other operating systems, the CVC4 library must be added manually)
- [Graphviz](https://www.graphviz.org/) 


    
To run the Repliss Demo webserver run:

    sbt "run --server"
    
Hostname and port can be configured with the `--host` and `--port` arguments.   
    

Other useful commands, which can be used in an SBT console:
    
    
  - Build an executable jar file with `assembly`
  - Compile with `compile`
  - Run tests with `test`
  - For continous building of the web server:
       - Run the main project with `~reStart --server`
       - Run the `js` subproject with `dev` and open port `8081`.
  - Run a specific file with `run <filename>`
    By default Repliss only parses and typechecks the file. 
    Use the `--quickcheck` option to enable automatic tests and the `--symbolicCheck` option to verify the input.
  
      For example:
        
        run verified/userbase.rpls --symbolicCheck --quickcheck    

