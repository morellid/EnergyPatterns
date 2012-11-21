namespace InstructionEnergy.Tools

type internal KernelBuilder() =
    static member BuildKernel(numOps) =
        let mutable kernel = "kernel void run(global float* input, global float* output) {\n
                                 float e1 = input[0];\n
                                 float e2 = 10;\n"
        for i = 0 to numOps - 1 do
            if (i % 2 = 0) then
                kernel <- kernel + "e1 += e2;\n";
            else
                kernel <- kernel + "e2 += e1;\n";
        
        if ((numOps - 1) % 2 = 0) then
            kernel <- kernel + "output[0] = e1;\n}\n";
        else
            kernel <- kernel + "output[0] = e2;\n}\n";

        kernel

        

