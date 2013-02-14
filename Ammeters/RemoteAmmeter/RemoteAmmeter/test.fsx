#I @"C:\Users\root\Desktop\EnergyPatterns\Ammeters\RemoteAmmeter\RemoteAmmeter\bin\Debug"
#r @"RemoteAmmeter.dll"
open EnergyPatterns.RemoteAmmeter

//#r @"Energon.Phidgets.dll"
//#r @"Phidget21.NET.dll"
//open Phidgets30A
//openPhidgets()
//let a = AmmeterSensor("TEST", 0, ifkit, 1.)

new Server("99") // workaround...

let s = new Server("1")
s.Start()
// now you can run experiments..

// when all the experiments are done, close the server:
s.Stop()

// test
let c = new Client("131.114.88.115", "1")
c.start()
c.stop()
