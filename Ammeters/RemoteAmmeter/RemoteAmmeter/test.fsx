#I @"C:\Users\root\Desktop\EnergyPatterns\Ammeters\RemoteAmmeter\RemoteAmmeter\bin\Debug"
#r @"RemoteAmmeter.dll"
open EnergyPatterns.RemoteAmmeter


let s = new Server()
s.Start()

let c = new Client("131.114.88.115")
c.start()

c.stop()

s.Stop()
