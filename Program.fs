type State =
    | CLOSED
    | LISTEN
    | SYN_SENT
    | SYN_RCVD
    | ESTABLISHED
    | CLOSE_WAIT
    | LAST_ACK
    | FIN_WAIT_1
    | FIN_WAIT_2
    | CLOSING
    | TIME_WAIT

let transitionTable =
    [|
        CLOSED, "APP_PASSIVE_OPEN", LISTEN;
        CLOSED, "APP_ACTIVE_OPEN", SYN_SENT;
        LISTEN, "RCV_SYN", SYN_RCVD;
        LISTEN, "APP_SEND", SYN_SENT;
        LISTEN, "APP_CLOSE", CLOSED;
        SYN_RCVD, "APP_CLOSE", FIN_WAIT_1;
        SYN_RCVD, "RCV_ACK", ESTABLISHED;
        SYN_SENT, "RCV_SYN", SYN_RCVD;
        SYN_SENT, "RCV_SYN_ACK", ESTABLISHED;
        SYN_SENT, "APP_CLOSE", CLOSED;
        ESTABLISHED, "APP_CLOSE", FIN_WAIT_1;
        ESTABLISHED, "RCV_FIN", CLOSE_WAIT;
        FIN_WAIT_1, "RCV_FIN", CLOSING;
        FIN_WAIT_1, "RCV_FIN_ACK", TIME_WAIT;
        FIN_WAIT_1, "RCV_ACK", FIN_WAIT_2;
        CLOSING, "RCV_ACK", TIME_WAIT;
        FIN_WAIT_2, "RCV_FIN", TIME_WAIT;
        TIME_WAIT, "APP_TIMEOUT", CLOSED;
        CLOSE_WAIT, "APP_CLOSE", LAST_ACK;
        LAST_ACK, "RCV_ACK", CLOSED
    |]

let rec findState (currentState: State) (events: string list) =
    match events with
    | [] -> Some currentState
    | event::rest ->
        let nextStateOption =
            transitionTable
            |> Array.tryFind (fun (state, evt, _) -> state = currentState && evt = event)
            |> Option.map (fun (_, _, nextState) -> nextState)
        match nextStateOption with
        | Some nextState -> findState nextState rest
        | None -> None


// вводить можно или через ";" без пробелов в формате "APP_PASSIVE_OPEN;APP_SEND;RCV_SYN_ACK"
// или каждое новое состояние с новой строки. Пустая строка является окончанием ввода
let rec readFromConsole () =
    let input = System.Console.ReadLine().Trim()
    if input = "" then []
    else
        let eventList = input.Split([|';'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
        eventList @ readFromConsole()

//По умолчанию при пустой входной строке возвращаю состояние CLOSED так как соеденение не открыто
let rec processEvents () =
    let events = readFromConsole()
    let result = findState CLOSED events
    match result with
    | Some state -> printfn "%A" state
    | None -> printfn "ERROR"
    processEvents()

processEvents()