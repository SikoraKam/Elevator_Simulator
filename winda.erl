-module(winda).
-compile(export_all).




main() ->
    Elevator_Pid = spawn(winda, run, [0]),
    Max_Floor_Pid = spawn(winda, max_floor, []),
    Error_Handler_Pid = spawn(winda, on_error, []),
    Listener_Pid = spawn(winda, listen, []),
    External_Pid = spawn(winda, external_manage, []),
    Internal_Pid = spawn(winda, internal_manage, []),
    
    ets:new(pids, [set, named_table]),
    ets:new(max_floor_var, [set, named_table, public]),
    ets:new(floors_var, [set, named_table, public]),
    ets:insert(pids, [{elevator_pid, Elevator_Pid},
        {start_floor_pid, Max_Floor_Pid},
        {error_pid, Error_Handler_Pid},{listener_pid, Listener_Pid},
        {external_pid, External_Pid}, {internal_pid, Internal_Pid}]),

    ets:new(elevator_stops_up, [set, named_table, public]),
    ets:new(elevator_stops_down, [set, named_table, public]),

    io:format("\nSYMULATOR WINDY\n"),
    timer:sleep(1000),
    Max_Floor_Pid ! {choose_max_floor}.

max_floor() -> 
    [{error_pid, Error_Handler_Pid}] = ets:lookup(pids, error_pid),
   
    receive
        {choose_max_floor} ->
            clear(),
            Floors_Number_Input = io:read("\nPodaj liczbe pieter \n Input: "),
            io:write(Floors_Number_Input),

             case Floors_Number_Input of
                 {ok,Max_Floor} -> 
                    if
                        Max_Floor < 1 ->
                             Error_Handler_Pid ! {wrong_max_floor_input};
                        true->
                            ets:insert(max_floor_var, {Max_Floor, Max_Floor}),
                            handle_calls_input(0)
                    end;
                 {error, _} ->
                     Error_Handler_Pid ! {wrong_max_floor_input},
                     max_floor()
             end
    end.

handle_calls_input(Actual_Floor) -> 
    [{elevator_pid,Elevator_Pid}] = ets:lookup(pids, elevator_pid),
    [{listener_pid, Listener_Pid}] = ets:lookup(pids, listener_pid),

    Elevator_Pid ! {run_elevator},
    {ok,Input} = io:read("\n Podaj wezwania dla windy w formacie listy krotek {pietro,kierunek},
        1 oznacza kierunek w gore, 0 wezwanie wewnatrz windy, -1 kierunek w dol,\n
        po wykonaniu dzialania windy bedziesz mogl wprowadzic kolejne wejscie \n
        Przyklad: [{3,1},{7,-1},{8,0}]. \n
        Aby wyjsc z programu napisz exit.
        Input: "),
    io:write(Input),
    if
        Input == exit ->
            Listener_Pid ! {exit_elevator};
        true ->
            handle_calls(Input,Actual_Floor)
    end.



handle_calls([], _) ->
    io:format("\n");


handle_calls([{Floor,Dir}|T],Actual_Floor)->
    [{external_pid, External_Pid}] = ets:lookup(pids, external_pid),
    [{internal_pid, Internal_Pid}] = ets:lookup(pids, internal_pid),
    [{error_pid, Error_Handler_Pid}] = ets:lookup(pids, error_pid),
    Max_Floor = ets:first(max_floor_var),
    if 
        ((Floor > -1) and (Floor =< Max_Floor)) -> 
            case Dir of
                1 -> 
                    External_Pid ! {external_call, Floor, Dir},
                    handle_calls(T,Actual_Floor);
                -1 -> 
                    External_Pid ! {external_call, Floor, Dir},
                    handle_calls(T,Actual_Floor);
                0 -> 
                    Internal_Pid ! {internal_call, Floor, Actual_Floor},
                    handle_calls(T,Actual_Floor);
                _ ->
                    Error_Handler_Pid ! {wrong_direction_input}
            end;
        true -> 
            Error_Handler_Pid ! {wrong_floor_input}
    end.

on_error()->
    receive
        {wrong_max_floor_input} -> 
            clear(),
            io:format("Wrong start floor input \n"),
            halt();
        {wrong_direction_input}->
            clear(),
            io:format("Wrong direction input \n"),
            halt();
        {wrong_floor_input}->
            io:format("Wrong floor input \n"),
            halt()
    end.

internal_manage()->
    receive
        {internal_call, Floor, Actual_Floor} ->
            if
                Floor > Actual_Floor ->
                    ets:insert(elevator_stops_up,{Floor,Floor});
                Floor < Actual_Floor ->
                    ets:insert(elevator_stops_down,{Floor,Floor})
            end,
            internal_manage()
    end.

external_manage()->
    receive
        {external_call, Floor, 1} -> 
            ets:insert(elevator_stops_up,{Floor,Floor}),
            external_manage();
        {external_call, Floor, -1} ->
            ets:insert(elevator_stops_down,{Floor,Floor}),
            external_manage()
    end.
         
run(Start_Floor)->
    receive 
          {run_elevator}->
              run_helper(Start_Floor, 0, 1)
    end.

run_helper(Start_Floor,Move,Dir)->
    [{listener_pid, Listener_Pid}] = ets:lookup(pids, listener_pid),

    timer:sleep(500),
    Actual_Floor = Start_Floor,
    Has_Stop_Above = hasStopAbove(Actual_Floor),
    Has_Stop_Below = hasStopBelow(Actual_Floor),
    Min_Set_Floor = getMinSetFloor(),
    Max_Set_Floor = getMaxSetFloor(),

    
    if
        ((Move == 0) and (Dir == 0)) ->
            if
                Has_Stop_Below == 0 ->
                    New_Dir = 1,
                    New_Move = Move;
                
            true ->
                New_Move = 1,
                New_Dir = Dir
            end,

            run_helper(Actual_Floor, New_Move, New_Dir);
        ((Move == 0) and (Dir == 1)) ->
            if
                Has_Stop_Above == 0 ->
                    New_Dir = 0,
                    New_Move = Move;
                
            true -> 
                New_Move = 1,
                New_Dir = Dir
            end,

            run_helper(Actual_Floor, New_Move, New_Dir);
        ((Move == 1) and (Dir == 0)) ->
            if
                Actual_Floor > Min_Set_Floor ->
                    New_Actual_Floor = Actual_Floor - 1,
                    io:fwrite("\n Floor: ~w \n", [New_Actual_Floor]),
                    New_Dir = Dir,
                    New_Move = Move,
                    
                    While_Moving_Down_Should_Stop_At = whileMovingDownShouldStopAt(New_Actual_Floor); 
            true -> 
                New_Move = 0,
                New_Dir = 1,
                New_Actual_Floor = Actual_Floor,
                While_Moving_Down_Should_Stop_At = whileMovingDownShouldStopAt(New_Actual_Floor)

            end,
            if
                ((While_Moving_Down_Should_Stop_At == New_Actual_Floor) or (New_Actual_Floor == Min_Set_Floor)) ->
                    New_Move_Second = 0,
                    New_Dir_Second = New_Dir,
                    clearStopDown(New_Actual_Floor),
                    clearStopUp(New_Actual_Floor),
                    io:format("\n STOP \n"),
                    Stops_Up = ets:tab2list(elevator_stops_up), 
                    Stops_Down = ets:tab2list(elevator_stops_down),
                    if 
                        ((Stops_Up == []) and (Stops_Down == [])) ->
                            Listener_Pid ! {elevator_work_finished, New_Actual_Floor},
                            run_helper(New_Actual_Floor, New_Move_Second, New_Dir_Second);
                        true ->
                             run_helper(New_Actual_Floor, New_Move_Second, New_Dir_Second)
                    end;
            true ->
                run_helper(New_Actual_Floor, New_Move, New_Dir)
            end;
            

        ((Move == 1) and (Dir == 1)) ->
                if
                    Actual_Floor < Max_Set_Floor ->
                        New_Actual_Floor = Actual_Floor + 1,
                        io:fwrite("\n Floor: ~w \n", [New_Actual_Floor]),
                        New_Move = Move,
                        New_Dir = Dir,
                         
                        While_Moving_Up_Should_Stop_At = whileMovingUpShouldStopAt(New_Actual_Floor);
                true ->
                    New_Move = 0,
                    New_Dir = 1,
                    New_Actual_Floor = Actual_Floor,
                    While_Moving_Up_Should_Stop_At = whileMovingUpShouldStopAt(New_Actual_Floor)

                end,
     
                if
                    ((While_Moving_Up_Should_Stop_At == New_Actual_Floor) or (New_Actual_Floor == Max_Set_Floor)) ->
                        New_Move_Second = 0,
                        New_Dir_Second = New_Dir,
                        clearStopDown(New_Actual_Floor),
                        clearStopUp(New_Actual_Floor),
                        io:format("\n STOP \n"),
                        Stops_Up = ets:tab2list(elevator_stops_up), 
                        Stops_Down = ets:tab2list(elevator_stops_down),
                        if 
                            ((Stops_Up == []) and (Stops_Down == [])) ->
                                Listener_Pid ! {elevator_work_finished, New_Actual_Floor},
                                run_helper(New_Actual_Floor, New_Move_Second, New_Dir_Second);

                        true ->
                             run_helper(New_Actual_Floor, New_Move_Second, New_Dir_Second)
                        end;    
                true ->
                    run_helper(New_Actual_Floor, New_Move, New_Dir)
                end;
        true ->
            run_helper(Actual_Floor, Move, Dir)
    end.


listen()->
    receive
        {elevator_work_finished, Actual_Floor} ->
            handle_calls_input(Actual_Floor),
            listen();
        {exit_elevator} ->
            io:format("\n KONIEC SYMULACJI \n"),
            halt()
    end.

clear() -> 
    io:format(os:cmd(clear)).
    

%%%%% ELEVATOR STOPS UTILS %%%%%

clearStopUp(Floor) ->
    ets:delete(elevator_stops_up, Floor).

clearStopDown(Floor) ->
    ets:delete(elevator_stops_down, Floor).

setLiftStopUp(Floor) ->
    ets:insert(elevator_stops_up, {Floor, Floor}).

setLiftStopDown(Floor) ->
    ets:insert(elevator_stops_down, {Floor, Floor}).

hasStopAbove(Curr_Floor) ->
    Indexes = lists:seq(Curr_Floor + 1, ets:first(max_floor_var)),
    hasStopAboveHelper(Indexes).

hasStopAboveHelper([]) -> 0; %zwraca boolean
hasStopAboveHelper([H|T])->
    StopsUpList = ets:tab2list(elevator_stops_up),
    StopsDownList = ets:tab2list(elevator_stops_down),
    StopUp = listFind({H,H}, StopsUpList),
    StopDown = listFind({H,H}, StopsDownList),
    if (StopUp or StopDown) ->
        1;
    true -> hasStopAboveHelper(T) %zwraca boolean
    end.

hasStopBelow(Curr_Floor) ->
    Indexes = lists:reverse(lists:seq(0,Curr_Floor -1)),
    hasStopBelowHelper(Indexes).

hasStopBelowHelper([]) -> 0; %zwraca boolean
hasStopBelowHelper([H|T])->
    StopsUpList = ets:tab2list(elevator_stops_up),
    StopsDownList = ets:tab2list(elevator_stops_down),
    StopUp = listFind({H,H}, StopsUpList),
    StopDown = listFind({H,H}, StopsDownList),
    if (StopUp or StopDown) ->
        1;
    true -> hasStopBelowHelper(T) %zwraca boolean
    end.

getMinSetFloorHelper([]) -> 0;
getMinSetFloorHelper([H|T]) -> 
    StopsUpList = ets:tab2list(elevator_stops_up),
    StopsDownList = ets:tab2list(elevator_stops_down),
    StopUp = listFind({H,H}, StopsUpList),
    StopDown = listFind({H,H}, StopsDownList),
    if (StopUp or StopDown) ->
        H;
    true -> getMinSetFloorHelper(T)
    end.

getMinSetFloor() -> 
    Indexes = lists:seq(0, ets:first(max_floor_var)),
    getMinSetFloorHelper(Indexes).

getMaxSetFloorHelper([]) -> 0;
getMaxSetFloorHelper([H|T]) ->
    StopsUpList = ets:tab2list(elevator_stops_up),
    StopsDownList = ets:tab2list(elevator_stops_down),
    StopUp = listFind({H,H}, StopsUpList),
    StopDown = listFind({H,H}, StopsDownList),
    if (StopUp or StopDown) ->
        H;
    true -> getMaxSetFloorHelper(T)
    end.

getMaxSetFloor() -> 
    Indexes = lists:reverse(lists:seq(0, ets:first(max_floor_var))),
    getMaxSetFloorHelper(Indexes).

whileMovingUpShouldStopAt(Curr_Floor) ->
    R = ets:lookup(elevator_stops_up, Curr_Floor),
    if 
        R == [] ->
            -1;
    true ->
        [{Key,_}] = R,
        Key
    end.

whileMovingDownShouldStopAt(Curr_Floor) ->
    R = ets:lookup(elevator_stops_down, Curr_Floor),
    if 
        R == [] ->
            -1;
    true ->
        [{Key,_}] = R,
        Key
    end.


listFind(Element, List) ->
  lists:member(Element, List).



     


    
    


