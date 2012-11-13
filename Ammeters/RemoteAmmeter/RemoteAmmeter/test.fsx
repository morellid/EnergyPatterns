#I @"C:\Users\root\Desktop\EnergyPatterns\Ammeters\RemoteAmmeter\RemoteAmmeter\bin\Debug"
#r @"RemoteAmmeter.dll"
open EnergyPatterns.RemoteAmmeter

let s = new Server()
s.Start()
// now you can run experiments..


// when all the experiments are done, close the server:
s.Stop()




// test
//let c = new Client("131.114.88.115")
//c.start()
//c.stop()
