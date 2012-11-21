namespace TransferEnergy.Tools

open Cloo
open System
open Microsoft.FSharp.Reflection
open System.Runtime.InteropServices
open TransferEnergy.Exceptions
open TransferEnergy.Data

module internal MemoryUtil =
    [<DllImport("kernel32.dll")>]
    extern void RtlMoveMemory(IntPtr dest, IntPtr src, uint32 len);
    [<DllImport("msvcrl.dll")>]
    extern int memcmp(IntPtr ptr1, IntPtr ptr2, int count);

module internal TransferTools =
    // Test copy from host ptr to host ptr
    let HostPtrToHostPtr(currSize, validate) = 
        // From array to array, via memcpy, including allocation and initialization
        let source = Array.create (currSize / sizeof<float32>) 2.0f
        let dest = Array.zeroCreate<float32> (currSize / sizeof<float32>)
        let sourceHandle = GCHandle.Alloc(source, GCHandleType.Pinned)
        let destHandle = GCHandle.Alloc(dest, GCHandleType.Pinned)
        try
            let sourcePtr = sourceHandle.AddrOfPinnedObject()
            let destPtr = destHandle.AddrOfPinnedObject()
            MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize)                            
            // Validate
            if validate && (MemoryUtil.memcmp(sourcePtr, destPtr, currSize) <> 0) then
                raise (new TransferException("Source and destination do not match"))
        finally
            if (sourceHandle.IsAllocated) then
                sourceHandle.Free()
            if (destHandle.IsAllocated) then
                destHandle.Free()

    // Test copy from host ptr to buffer
    let HostPtrToBuffer(computeContext:ComputeContext, computeQueue:ComputeCommandQueue, currSize, validate, dstInfo:TransferEndpoint) =
        let src = Array.create (currSize / sizeof<float32>) 2.0f
        let dst = new ComputeBuffer<float32>(computeContext, dstInfo.Flags, (int64)(currSize / sizeof<float32>))

        if dstInfo.ShouldMap then
            // From array to buffer, via map and memcpy, including allocation and initialization
            let sourceHandle = GCHandle.Alloc(src, GCHandleType.Pinned)
            try 
                let sourcePtr = sourceHandle.AddrOfPinnedObject()
                let destPtr = computeQueue.Map<float32>(dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null)
                MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize)                                   
                computeQueue.Unmap(dst, ref destPtr, null)                             
            finally
                if (sourceHandle.IsAllocated) then
                    sourceHandle.Free()
        else
            // From array to buffer, via writeBuffer, including allocation and initialization
            computeQueue.WriteToBuffer<float32>(src, dst, true, null)
                        
        // Validate            
        if validate then
            let finalizer = Array.zeroCreate<float32>(currSize / sizeof<float32>)
            computeQueue.ReadFromBuffer<float32>(dst, ref finalizer, true, null)
            Array.iteri (fun i element -> 
                if element <> finalizer.[i] then
                    raise (new TransferException("Source and destination do not match"))) src   

    // Test copy from buffer to host ptr
    let BufferToHostPtr<'T>(computeContext:ComputeContext, computeQueue:ComputeCommandQueue, currSize, validate, srcInfo:TransferEndpoint) =
        let src = new ComputeBuffer<float32>(computeContext, srcInfo.Flags, (int64)(currSize / sizeof<float32>))
        let dst = Array.zeroCreate<float32>(currSize / sizeof<float32>)            
        // Init source
        let initializer = Array.create<float32> (currSize / sizeof<float32>) 2.0f
        computeQueue.WriteToBuffer<float32>(initializer, src, true, null)

        if srcInfo.ShouldMap then
            // From buffer to array, via map and memcpy, including allocation
            let destHandle = GCHandle.Alloc(dst, GCHandleType.Pinned)
            try 
                let destPtr = destHandle.AddrOfPinnedObject()
                let sourcePtr = computeQueue.Map<float32>(src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null)
                MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize) 
                computeQueue.Unmap(src, ref sourcePtr, null)                                                                 
            finally
                if (destHandle.IsAllocated) then
                    destHandle.Free() 
        else
            computeQueue.ReadFromBuffer<float32>(src, ref dst, true, null)

        // Validate
        if validate then
            Array.iteri (fun i element -> 
                if element <> initializer.[i] then
                    raise (new TransferException("Source and destination do not match"))) dst                                   
                
    // Test copy from buffer to buffer
    let BufferToBuffer<'T>(computeContext:ComputeContext, computeQueue:ComputeCommandQueue, currSize, validate, srcInfo:TransferEndpoint, dstInfo:TransferEndpoint) =
        let dst = new ComputeBuffer<float32>(computeContext, dstInfo.Flags, (int64)(currSize / sizeof<float32>))
        let src = new ComputeBuffer<float32>(computeContext, srcInfo.Flags, (int64)(currSize / sizeof<float32>))
        
        // Init source
        let initializer = Array.create<float32> (currSize / sizeof<float32>) 2.0f
        computeQueue.WriteToBuffer<float32>(initializer, src, true, null)

        if srcInfo.ShouldMap && dstInfo.ShouldMap then
            // From buffer to buffer, via map and memcpy, including allocation
            let destPtr = computeQueue.Map<float32>(dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null)
            let sourcePtr = computeQueue.Map<float32>(src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null)
            MemoryUtil.RtlMoveMemory(sourcePtr, destPtr, (uint32)currSize) 
            computeQueue.Unmap(dst, ref destPtr, null)            
            computeQueue.Unmap(src, ref sourcePtr, null)    
        elif srcInfo.ShouldMap && (not dstInfo.ShouldMap) then
            // From buffer to buffer, via writeBuffer, including allocation
            let sourcePtr = computeQueue.Map<float32>(src, true, ComputeMemoryMappingFlags.Read, (int64)0, (int64)(currSize / sizeof<float32>), null)
            computeQueue.Write<float32>(dst, true, 0L, (int64)(currSize / sizeof<float32>), sourcePtr, null)
            computeQueue.Unmap(src, ref sourcePtr, null)  
        elif (not srcInfo.ShouldMap) && dstInfo.ShouldMap then
            // From buffer to buffer, via readBuffer, including allocation
            let destPtr = computeQueue.Map<float32>(dst, true, ComputeMemoryMappingFlags.Write, (int64)0, (int64)(currSize / sizeof<float32>), null)
            computeQueue.Read<float32>(src, true, 0L, (int64)(currSize / sizeof<float32>), destPtr, null)
            computeQueue.Unmap(src, ref destPtr, null)
        else                                                    
            // From buffer to buffer, via buffer copy, including allocation
            computeQueue.CopyBuffer<float32>(src, dst, null)  
            
        // Validate
        if validate then
            let finalizer = Array.zeroCreate<float32>(currSize / sizeof<float32>)
            computeQueue.ReadFromBuffer<float32>(dst, ref finalizer, true, null)
            Array.iteri (fun i element -> 
                if element <> initializer.[i] then
                    raise (new TransferException("Source and destination do not match"))) finalizer       
 
    