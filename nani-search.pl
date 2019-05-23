
:- dynamic 
    here/1,
    location/2,
    has/1,
    on/1,
    locked/2,
    hidden_in/1,
    location_list/2,
    off/1.

:- op(20, fx, take).
:- op(20, fx, goto).
:- op(20, fx, drop).
:- op(20, fx, put_down).
:- op(20, fx, eat).
:- op(20, fx, unlock).
:- op(20, fx, drink).
:- op(20, fx, hide).



% CUARTOS
room(reciever).
room(kitchen).
room(garage).
room(garden).
room('living room').
room('dinning room').
room(laundry).
room('main access').
room('reciever wc').
room('storage room').
room('bedroom mom').
room('bedroom brother').
room('upstairs wc').
room('upstairs lobby').
room('music room').
room(staircase).
room(backyard).

on(reciever).
on(kitchen).
on(garage).
on(garden).
on('dinning room').
on('main access').
on('reciever wc').
on('bedroom mom').
on('upstairs wc').
on('upstairs lobby').
on('music room').
on(backyard).

off(staircase).
off(laundry).
off('storage room').
off('bedroom brother').
off('living room').


% COSAS
location('key holder', reciever).
location('coat rack', reciever).
location('glass table', reciever).

location(object(table, beige, 13), kitchen).
location(fridge, kitchen).
location(milk, kitchen).
location(sink, kitchen).
location(pantry, kitchen).
location(stove, kitchen).
location(microwave, kitchen).
location(apple, kitchen).
location(knife, kitchen).

location(object(car, green, 140), garage).
location(karcher, garage).
location(object(bike, yellow, 3), garage).

location(flowers, garden).
location(bench, garden).
location(object(hoose, green, 8), garden).

location(object(sofa, purple, 8), 'living on').
location(lamp, 'living room').
location(tv, 'living room').

location(object(table, brown, 24), 'dinning room').
location(object(piano, brown, 50), 'dinning room').
location(photograph, piano).
location(cabinet, 'dinning room').

location(washer, laundry).
location(object(soap, lavender, 0.2), washer).
location(sink, laundry).
location(toolbox, laundry).

location('main door', 'main access').
location(flowers, 'main access').
location('main access keys', flowers).
location(object(mat, white, 0.7), 'main access').
location(object(bench, white, 100), 'main access').

location(toilet, 'reciever wc').
location(sink, 'reciever wc').
location(shower, 'reciever wc').

location('old fridge', 'storage room').
location('broken table', 'storage room').
location(object(corpse, black, 50), 'storage room').
location(object(key, golden, 0.2), corpse).

location(object(bed, white, 30), 'bedroom brother').
location(object(desk, brown, 40), 'bedroom brother').
location(closet, 'bedroom brother').
location('secret box', 'bedroom brother').

location(bed, 'bedroom mom').
location(cabinet, 'bedroom mom').
location(closet, 'bedroom mom').

location(toilet, 'upstairs wc').
location(shower, 'upstairs wc').
location(drawer, 'upstairs wc').

location(window, staircase).
location(plant, staircase).

location(photos, 'upstairs lobby').
location(drawer, 'upstairs lobby').
location(couch, 'upstairs lobby').

location(object(guitar, red, 5), 'music room').
location(object(drumset, crimson, 6.4), 'music room').
location(keyboard, 'music room').
location(object(microphone, black, 2), 'music room').
location(object(amplifier, black, 2.7), 'music room').

location(object(ball, blue, 0.5), backyard).
location(dog, backyard).
location(object(tree, green, 100), backyard).


location_list([chair, 'mobile table', lock], kitchen).
location_list([dogpoo, leaf], 'main access').
location_list([cross, 'broken glass'], staricase).
location_list([hammock, bench, pot], backyard).
location_list([weed, cheeto], 'music room').
location_list([controller, cushion], 'living room').


door('main access', reciever).
door(garden, 'main access').
door(backyard, garden).
door('reciever wc', reciever).
door('living room', reciever).
door('main access', garage).
door('dinning room', 'living room').
door(kitchen, 'dinning room').
door(laundry, kitchen).
door(kitchen, garage).
door(laundry, backyard).
door(backyard, 'living room').
door('living room', staircase).
door(staircase, 'upstairs lobby').
door('upstairs lobby', 'bedroom brother').
door('bedroom brother', 'music room').
door('music room', backyard).
door(backyard, 'storage room').
door('bedroom mom', 'upstairs lobby').
door('upstairs lobby', 'upstairs wc').

locked(backyard, 'storage room').
locked('upstairs lobby', 'bedroom mom').
locked(garage, kitchen).

hide_spot(fridge, kitchen).
hide_spot(cabinet, 'bedroom mom').
hide_spot(closet, 'bedroom mom').
hide_spot('old fridge', 'storage room').
hide_spot(toilet, 'reciever wc').
hide_spot(cabinet, 'dinning room').

hide(Where) :-
    not(hidden_in(Where)),
    asserta(hidden_in(Where)),
    write("You hid in the "),
    write(Where),
    !.

come_out :-
    hidden_in(Where),
    retract(hidden_in(Where)),
    write("You came out"),
    !.

connection(X, Y) :- door(X, Y).
connection(X, Y) :- door(Y, X).



edible(apple).
edible(flowers).
drinkable(milk).
tastes_yucky(flowers).


where_food(X, Y) :-
    is_contained_in(X, Y),
    edible(X).
where_food(X, Y) :-
    is_contained_in(X, Y),
    tastes_yucky(X).



list_things(Place) :-
    is_contained_in(Thing, Place),
    atom(Thing), % to avoid repeating complex things
    tab(2),
    write("- "),
    write(Thing),
    nl,
    fail. 
list_things(Place) :-
    is_contained_in(object(Thing, Color, Weight),Place),
    tab(2),
    write("- "),
    write_object(Thing, Color, Weight),
    nl,
    fail.

list_things(_).


list_connections(Place) :-
    connection(Place, X),
    tab(2),
    write("- "),
    write(X),
    nl,
    fail.
list_connections(_).

here('main access').

write_object(Thing, Color, Weight):-
    write('A '),
    write(Color),tab(1),
    write(Thing), write(', weighing '),
    write(Weight), write(' pounds').



look :- 
    here(Place),
    on(Place),
    write('You are in the '),
    write(Place),
    nl,
    write('You can see:'),
    nl, list_things(Place),
    write('You can go to:'),
    nl, list_connections(Place),
    !.
look :-
    write("The light is turned off").


goto(Place) :-
    can_go(Place),
    retract(here(_)),
    asserta(here(Place)),
    write("You moved to: "),
    write(Place),
    nl,
    look,
    !.


can_go(To) :-
    here(From),
    connection(To, From),
    not(locked(From, To)).

can_go(Place) :- % como un else
    here(X),
    not(connection(Place, X)),
    write("You can't get there from here!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover


can_go(Place) :- % como un else
    here(X),
    locked(X, Place),
    write("The door is locked"),
    nl,
    fail. % para que no la mueva cuando no se puede mover



take(Thing) :-
    here(Where),
    location(Thing, Where),
    retract(location(Thing, Where)),
    asserta(has(Thing)),
    write("You took: "),
    write(Thing),
    nl,
    !.

take(Thing) :-
    here(Where),
    location_list(List, Where),
    member(Thing, List),
    remove(Thing, List, Remaining),
    retract(location_list(List, Where)),
    asserta(location_list(Remaining, Where)),
    asserta(has(Thing)),
    write("You took: "),
    write(Thing),
    nl,
    !.


% can_take(Thing) :-
%     here(Where),
%     location(Thing, Where).


% can_take(Thing) :-
%     here(Where),
%     location_list(List, Where),
%     member(Thing, List).

% can_take(_) :- % como un else
%     write("There is no object with that name!"),
%     nl,
%     fail. % para que no la mueva cuando no se puede mover



take(Thing) :-
    here(Where),
    location(object(Thing, _, _), Where),
    retract(location(object(Thing, Color, Weight), Where)),
    asserta(has(object(Thing, Color, Weight))),
    write("You took: "),
    write(Thing),
    nl,
    !.



take(_):-
    write("There is no object with that name!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover
    

drop(Thing) :-
    has(Thing),
    retract(has(Thing)),
    % no se aserta nada porque se rompe el objeto
    write("You dropped: "),
    write(Thing),
    !.

drop(Thing) :-
    has(object(Thing, Color, Weight)),
    retract(has(object(Thing, Color, Weight))),
    % no se aserta nada porque se rompe el objeto
    write("You dropped: "),
    write(Thing),
    !.

drop(_) :- % como un else
    write("You don't have that!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover



drop_object(Thing) :-
    can_drop_object(Thing),
    retract(has(object(Thing, _, _))),
    % no se aserta nada porque se rompe el objeto
    write("You dropped: "),
    write(Thing),
    !.
can_drop_object(Thing) :-
    has(object(Thing, _, _)).
can_drop_object(_) :- % como un else
    write("You don't have that object!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover
    
    


put_down(Thing) :-
    has(Thing),
    here(Where),
    retract(has(Thing)),
    asserta(location(Thing, Where)),
    write("You put down: "),
    write(Thing),
    !.

put_down(Thing) :-
    here(Where),
    has(object(Thing, Color, Weight)),
    retract(has(object(Thing, Color, Weight))),
    asserta(location(object(Thing, Color, Weight), Where)),
    write("You put down: "),
    write(Thing),
    !.

put_down(_) :- % como un else
    write("You don't have that!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover
    

inventory :-
    not(has(_)),
    write("You don't have anything"),
    !.

inventory :-
    write("You have: "),
    write_item(_).
    

write_item(Thing):-
    has(Thing),
    atom(Thing),
    nl,
    write(" - "),
    write(Thing),
    fail. 

write_item(Thing):-
    has(object(Thing, Color, Weight)),
    nl,
    write(" - "),
    write_object(Thing, Color, Weight),
    fail. 


eat(Food) :-
    has(Food),
    edible(Food),
    retract(has(Food)),
    write("You ate: "),
    write(Food),
    !. 
eat(_) :-
    write("You can't eat that, honey!").

drink(Food) :-
    has(Food),
    drinkable(Food),
    retract(has(Food)),
    write("You drank: "),
    write(Food),
    !. 
drink(_) :-
    write("You can't drink that, honey!").


turn_on :-
    here(Where),
    off(Where),
    retract(off(Where)),
    asserta(on(Where)),
    write("You turned on the light"),
    !.
turn_on :-
    write("The light is already on").

turn_off :-
    here(Where),
    on(Where),
    retract(on(Where)),
    asserta(off(Where)),
    write("You turned off the light"),
    !.
turn_off :-
    write("The light is already off").

unlock(To) :-
    here(Where),
    retract(locked(Where, To)),
    write("You unlocked the door from the "),
    write(Where),
    write(" to the "),
    write(To),
    !.


is_contained_in(T1,T2) :-
    location(T1,T2).
is_contained_in(T1, T2):-
    location_list(List, T2),
    member(T1, List),
    atom(T1).
% is_contained_in(T1,T2) :-
%     location(X,T2),
%     is_contained_in(T1,X).



member(H, [H | _]).
member(X, [_ | T]):-
    member(X, T).


remove(H, [H | T], T).

remove(Thing, [H | T1], [H | T2]):-
    remove(Thing, T1, T2).