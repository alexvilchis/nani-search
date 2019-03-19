
:- dynamic 
    here/1,
    location/2,
    has/1,
    on/1,
    locked/2,
    hidden_in/1,
    off/1.



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

location(thing(table, beige, 13), kitchen).
location(fridge, kitchen).
location(milk, kitchen).
location(sink, kitchen).
location(pantry, kitchen).
location(stove, kitchen).
location(microwave, kitchen).
location(apple, kitchen).
location(knife, kitchen).

location(thing(car, green, 140), garage).
location(karcher, garage).
location(thing(bike, yellow, 3), garage).

location(flowers, garden).
location(bench, garden).
location(hoose, garden).

location(thing(sofa, purple, 8), 'living on').
location(lamp, 'living room').
location(tv, 'living room').

location(thing(table, brown, 24), 'dinning room').
location(thing(piano, brown, 50), 'dinning room').
location(photograph, piano).
location(cabinet, 'dinning room').

location(washer, laundry).
location(soap, washer).
location(sink, laundry).
location(toolbox, laundry).

location('main door', 'main access').
location(thing(bench, white, 100), 'main access').
location(flowers, 'main access').
location('main access keys', flowers).
location(mat, 'main access').

location(toilet, 'reciever wc').
location(sink, 'reciever wc').
location(shower, 'reciever wc').

location('old fridge', 'storage room').
location('broken table', 'storage room').
location(thing(corpse, black, 50), 'storage room').
location(thing(key, golden, 0.2), corpse).

location(thing(bed, white, 30), 'bedroom brother').
location(thing(desk, brown, 40), 'bedroom brother').
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

location(thing(guitar, red, 5), 'music room').
location(drumset, 'music room').
location(keyboard, 'music room').
location(thing(microphone, black, 2), 'music room').
location(amplifier, 'music room').

location(hammock, backyard).
location(thing(ball, blue, 0.5), backyard).
location(dog, backyard).
location(thing(tree, green, 100), backyard).


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
    is_contained_in(thing(Thing, Color, Weight),Place),
    tab(2),
    write("- "),
    write('A '),
    write(Color),tab(1),
    write(Thing), write(', weighing '),
    write(Weight), write(' pounds'), nl,
    fail.
list_things(Place) :-
    is_contained_in(Thing, Place),
    atom(Thing), % to avoid repeating complex things
    tab(2),
    write("- "),
    write(Thing),
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
    can_take(Thing),
    here(Where),
    retract(location(Thing, Where)),
    asserta(has(Thing)),
    write("You took: "),
    write(Thing),
    nl,
    !.

can_take(Thing) :-
    here(Where),
    location(Thing, Where).

can_take(Thing) :-
    here(Room),
    location(thing(Thing, _, small,_), Room).

can_take(_) :- % como un else
    write("There is no object with that name!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover




drop(Thing) :-
    can_drop(Thing),
    retract(has(Thing)),
    % no se aserta nada porque se rompe el objeto
    write("You dropped: "),
    write(Thing),
    !.

can_drop(Thing) :-
    has(Thing).

can_drop(_) :- % como un else
    write("You don't have that!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover




put_down(Thing) :-
    can_put_down(Thing),
    here(Where),
    retract(has(Thing)),
    asserta(location(Thing, Where)),
    write("You put down: "),
    write(Thing),
    !.

can_put_down(Thing) :-
    has(Thing).
can_put_down(_) :- % como un else
    write("You don't have that!"),
    nl,
    fail. % para que no la mueva cuando no se puede mover


inventory :-
    write("You have: "),
    nl,
    has(Thing),
    write(" - "),
    write(Thing),
    nl,
    fail. 
inventory :-
    write("You don't have anything, honey").


eat(Food) :-
    has(Food),
    edible(Food),
    retract(has(Food)),
    write("You ate: "),
    write(Food). 
eat(_) :-
    write("You can't eat that, honey!").

drink(Food) :-
    has(Food),
    drinkable(Food),
    retract(has(Food)),
    write("You drank: "),
    write(Food). 
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

unlock(From, To) :-
    retract(locked(From, To)),
    write("You unlocked the door from the "),
    write(From),
    write(" to the "),
    write(To),
    !.


is_contained_in(T1,T2) :-
    location(T1,T2).
is_contained_in(T1,T2) :-
    location(X,T2),
    is_contained_in(T1,X).