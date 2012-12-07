namespace ComputationDensity.Tools

type internal KernelBuilder() =
    static member BuildLoopInstructionKernel() =
        let mutable kernel = "kernel void run(global float* input, global float* output, int n, int a) {\n                                 
                                 volatile float e1 = input[a];\n
                                 volatile float e2 = 10;\n
                                 for(int i = 0; i <= n; i++) {\n"
        kernel <- kernel + "e1 += e2;\n";
        kernel <- kernel + "e2 += e1;\n";
        
        kernel <- kernel + "}\n"
        kernel <- kernel + "output[0] = e2;\n}\n";

        kernel
        
    static member BuildMemoryAccessKernel() =
        let mutable kernel = "kernel void run(global float* input, global float* output, int n) {\n                                 
                                 volatile float e1 = input[0];\n
                                 volatile float e2 = 10;\n
                                 for(int i = 0; i <= n; i++) {\n"
        kernel <- kernel + "e1 += e2;\n";
        kernel <- kernel + "e2 += e1;\n";
        
        kernel <- kernel + "}\n"
        kernel <- kernel + "output[0] = e2;\n}\n";

        kernel
        

