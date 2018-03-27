module burner.EnergyController

open Com.Enterprisecoding.RPI.GPIO
open Com.Enterprisecoding.RPI.GPIO.Enums

type EnergyDataFields = { TimeStamp: int64
                          Value: int }

type EnergyData =
| Input of EnergyDataFields
| Output of EnergyDataFields

type Power = {
    Input: int
    Output: int
}

let result = WiringPi.Core.Setup()
match result with
| -1 -> failwith "Setup WiringPi failed"
| _ -> ()

WiringPi.Core.PinMode(0, PinMode.Output)
WiringPi.Core.PinMode(2, PinMode.Output)
WiringPi.Core.PinMode(1, PinMode.PwmOutput)

let heatingController = MailboxProcessor.Start(fun inbox ->
    let (|High|InRange|) x =
        if(x > 2000) then High
        else InRange

    let highGuard x =
        match x with
        | High -> 2000
        | InRange -> x

    let translateToPwm x =
        2000 / 1024 * x

    let rec loop() = async {
        let! msg = inbox.Receive()
        printfn "Input %i" msg.Input
        printfn "Output %i" msg.Output

        match msg.Input > 0 with
        |true -> WiringPi.Core.DigitalWrite(0, DigitalValue.High) // Digital Write 1
        | _ -> WiringPi.Core.DigitalWrite(0, DigitalValue.Low) //Digital Write 0

        match msg.Output > 0 with
        |true -> WiringPi.Core.DigitalWrite(2, DigitalValue.High) // Digital Write 1
        | _ -> WiringPi.Core.DigitalWrite(2, DigitalValue.Low) //Digital Write 0

        //Write Output to PWM 0 - 2000
        let pwmValue = msg.Output
                       |> highGuard
                       |> translateToPwm
        WiringPi.Core.PWMWrite(1, pwmValue)

        return! loop()
    }

    loop()
)

let energyDataProcessor = MailboxProcessor.Start(fun inbox->
    let initialInput = Input {TimeStamp = 0L
                              Value = 0}
    let initialOutput = Output {TimeStamp = 0L
                                Value = 0}
    let unwrapData data =
        match data with
        | Input a -> a
        | Output a -> a

    let currentPower last current =
        let lastFields = unwrapData last
        let currentFields = unwrapData current

        let timeDiff = float (currentFields.TimeStamp - lastFields.TimeStamp)
        let powerDiff = float (int64(currentFields.Value - lastFields.Value))

        let powerPerMs = powerDiff / timeDiff
        int (powerPerMs)

    let rec messageLoop input output inputPower outputPower = async {
        // read a message
        let! msg = inbox.Receive()

        match msg with
        | Input a -> match a.TimeStamp with
                     | 0L -> return! messageLoop msg output inputPower outputPower
                     | _ -> let inputPower = currentPower input msg
                            heatingController.Post {Input = inputPower
                                                    Output = outputPower}
                            return! messageLoop msg output inputPower outputPower
        | Output a -> match a.TimeStamp with
                      | 0L -> return! messageLoop input  msg  inputPower  outputPower
                      | _ -> let outputPower = currentPower output msg
                             heatingController.Post {Input = inputPower
                                                     Output = outputPower}
                             return! messageLoop msg output inputPower outputPower
    }

    // start the loop
    messageLoop initialInput initialOutput 0 0
)