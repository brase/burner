module burner.EnergyController

open Com.Enterprisecoding.RPI.GPIO
open Com.Enterprisecoding.RPI.GPIO.Enums

type EnergyDataFields = { TimeStamp: int64
                          Value: float }

type EnergyData =
| Input of EnergyDataFields
| Output of EnergyDataFields

type Power = {
    Input: float
    Output: float
}

let result = WiringPi.Core.Setup()
match result with
| -1 -> failwith "Setup WiringPi failed"
| _ -> printfn "Setup WiringpPi successful: %i" result

WiringPi.Core.PinMode(0, PinMode.Output)
WiringPi.Core.PinMode(2, PinMode.Output)
WiringPi.Core.PinMode(1, PinMode.PwmOutput)

//WiringPi.OnBoardHardware.PwmSetMode(int PWMMode.MS)
//WiringPi.OnBoardHardware.PwmSetClock(192)
//WiringPi.OnBoardHardware.PwmSetRange(1023u)

printfn "Initialized WiringPi"

let heatingController = MailboxProcessor.Start(fun inbox ->
    let (|High|InRange|) x =
        if(x > 2000.) then High
        else InRange

    let highGuard x =
        match x with
        | High -> 2000.
        | InRange -> x

    let translateToPwm x =
        int (1024. / 2000. * x)

    let rec loop() = async {
        let! msg = inbox.Receive()
        printfn "Input %f W" msg.Input
        printfn "Output %f W" msg.Output

        match msg.Input > 0. with
        |true -> printfn "Input: True" //WiringPi.Core.DigitalWrite(0, DigitalValue.Low)
        | _ -> printfn "Input: False" //WiringPi.Core.DigitalWrite(0, DigitalValue.High)

        match msg.Output > 0. with
        |true -> printfn "Output: True" //WiringPi.Core.DigitalWrite(2, DigitalValue.Low)
        | _ -> printfn "Output: False"//WiringPi.Core.DigitalWrite(2, DigitalValue.High)

        let pwmValue = msg.Output
                       |> highGuard
                       |> translateToPwm
        //WiringPi.Core.PWMWrite(1, pwmValue)
        printfn "PWM: %i" pwmValue

        return! loop()
    }

    loop()
)

let energyDataProcessor = MailboxProcessor.Start(fun inbox->
    let initialInput = Input {TimeStamp = 0L
                              Value = 0.}
    let initialOutput = Output {TimeStamp = 0L
                                Value = 0.}
    let unwrapData data =
        match data with
        | Input a -> a
        | Output a -> a

    let currentPower last current =
        let lastFields = unwrapData last
        let currentFields = unwrapData current

        let timeDiff = float (currentFields.TimeStamp - lastFields.TimeStamp) //ms
                       / 1000. //s
        let powerDiff = (currentFields.Value - lastFields.Value) //KWh
                        * 1000. //Wh
                        * 3600. //Ws

        printfn "Timediff: %f s" timeDiff
        printfn "Powerdiff: %f Ws" powerDiff

        let power = powerDiff / timeDiff
        power

    let rec messageLoop input output inputPower outputPower = async {
        // read a message
        let! msg = inbox.Receive()

        match msg with
        | Input a -> let inputFields = unwrapData input
                     match inputFields.TimeStamp with
                     | 0L -> return! messageLoop msg output inputPower outputPower
                     | _ -> let inputPower = currentPower input msg
                            heatingController.Post {Input = inputPower
                                                    Output = outputPower}
                            return! messageLoop msg output inputPower outputPower
        | Output a -> let outputFields = unwrapData output
                      match outputFields.TimeStamp with
                      | 0L -> return! messageLoop input  msg  inputPower  outputPower
                      | _ -> let outputPower = currentPower output msg
                             heatingController.Post {Input = inputPower
                                                     Output = outputPower}
                             return! messageLoop msg output inputPower outputPower
    }

    // start the loop
    messageLoop initialInput initialOutput 0. 0.
)