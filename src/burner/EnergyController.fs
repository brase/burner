module burner.EnergyController

open Unosquare.RaspberryIO
open Unosquare.RaspberryIO.Abstractions
open Unosquare.WiringPi

type EnergyDataFields = { TimeStamp: int64
                          Value: float }

type EnergyData =
| Input of EnergyDataFields
| Output of EnergyDataFields

type Power = {
    Input: float
    Output: float
}

let heatingController = MailboxProcessor.Start(fun inbox ->
    let input = Pi.Gpio.[0]
    input.PinMode <- GpioPinDriveMode.Output

    let output = Pi.Gpio.[2]
    output.PinMode <- GpioPinDriveMode.Output

    let pwm = Pi.Gpio.[1]  :?> Unosquare.WiringPi.GpioPin
    pwm.PinMode <- GpioPinDriveMode.PwmOutput
    pwm.PwmMode <- PwmMode.MarkSign
    pwm.PwmClockDivisor <- 192
    pwm.PwmRange <- 1023u

    printfn "Initialized WiringPi"

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

        input.Write(msg.Input <= 0.)
        output.Write(msg.Output <= 0.)

        let pwmValue = msg.Output
                       |> highGuard
                       |> translateToPwm
        pwm.PwmRegister <- pwmValue
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
        | Input _ -> let inputFields = unwrapData input
                     match inputFields.TimeStamp with
                     | 0L -> return! messageLoop msg output inputPower outputPower
                     | _ -> let inputPower = currentPower input msg
                            heatingController.Post {Input = inputPower
                                                    Output = outputPower}
                            return! messageLoop msg output inputPower outputPower
        | Output _ -> let outputFields = unwrapData output
                      match outputFields.TimeStamp with
                      | 0L -> return! messageLoop input msg inputPower outputPower
                      | _ -> let outputPower = currentPower output msg
                             heatingController.Post {Input = inputPower
                                                     Output = outputPower}
                             return! messageLoop input msg inputPower outputPower
    }

    // start the loop
    messageLoop initialInput initialOutput 0. 0.
)