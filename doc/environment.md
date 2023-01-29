
# Environment

I have installed the Altera 13.1 Web Edition on a Windows host
to get access to the Quartus II command line tools
that support MAX II devices.

I have elected to run my experiments in an Erlang shell.
Since that is not very nice on Windows,
I have split it into two nodes.
The interactive node ([experiments](/experiments))
I am running on a NetBSD host and that sends requests to
a Windows node ([windows](/windows))
that executes the quartus compiler. 

## Windows

On the Windows side:

    C:\max-ii-fuse-map\windows> make

## Experiments

On the experiment side

    $ gmake
    $ gmake shell
    > run(test_experiment).

