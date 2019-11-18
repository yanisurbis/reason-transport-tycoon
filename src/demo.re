
type location = Factory | Port | A | B;
type activity = Move | Wait;
type category = Car | Ship;
type uuid = string;

type action = {
    start: location,
    finish: location,
    duration: int,
    activity: activity,
};

type actor = {
    id: uuid,
    category: category,
    location: location
};

// TODO: rename to deliveries
type element = {
    destination: location
};

type event = {
    id: uuid,
    actorId: string,
    payload: option(element),
    action: action,
    startTime: int,
    endTime: option(int)
}

type queue = {
    location: location,
    elements: list(element),
    final: bool,
}

type system = {
    amountToDeliver: int,
    currentTime: int,
    queues: list(queue),
    events: list(event),
    actors: list(actor),
    history: list(event),
}

// ----------------------------------------------
let convertLocation = (location: location) => Js.Json.string(switch(location) {
        | Port => "Port"
        | A => "A"
        | B => "B"
        | Factory => "Factory"
    })

let convertActivity = (a: activity) => Js.Json.string(switch(a) {
        | Move => "Move"
        | Wait => "Wait"
    })

let encodeAction = (a: action) => {
    Json.Encode.(object_([("duration", Json.Encode.int(a.duration)),
                        ("start", convertLocation(a.start)),
                        ("finish", convertLocation(a.finish)),
                        ("activity", convertActivity(a.activity))]));
}

let encodeElement = (e: element) => {
    Json.Encode.(object_([("destination", convertLocation(e.destination))]));
}

let encodeEvent = (e: event) => {
    Json.Encode.(object_([("action", encodeAction(e.action)),
                        ("startTime", Json.Encode.int(e.startTime)),  
                        ("endTime", switch(e.endTime) {
                            | Some(x) => Json.Encode.int(x)
                            | None => Json.Encode.string("*")
                        }),
                        ("actorId", Json.Encode.string(e.actorId)),
                        ("payload", switch(e.payload) {
                            | Some(x) => encodeElement(x)
                            | None => Json.Encode.string("*")
                        }),]));
}

// ----------------------------------------------

let elementsToDeliver: list(element) = List.map(x => {destination: x}, [A, A, B, A, B, B, A, B]);

let car1 = {
    id: "car1",
    category: Car,
    location: Factory
}
let car2 = {...car1, id: "car2"}
let ship1 = {
    id: "ship1",
    category: Ship,
    location: Port,
}

let system = {
    amountToDeliver: List.length(elementsToDeliver),
    currentTime:0,
    queues: [{location: Factory, elements: elementsToDeliver, final: false},
            {location: Port, elements: [], final: false},
            {location: A, elements: [], final: true},
            {location: B, elements: [], final: true}],
    events: [],
    history: [],
    actors: [car1, car2, ship1]
}

// ----------------------------------------------

let isEventCompleted = (system, event) => event.startTime + event.action.duration <= system.currentTime;
let getCompletedEvents = system => List.filter(isEventCompleted(system), system.events)
let getEventActor = (system, event) => List.find((actor:actor) => actor.id == event.actorId, system.actors)
let getFreeActors = system => {
    let busyActorsIds = system.events
        |> List.filter(event => !isEventCompleted(system, event))
        |> List.map(event => event.actorId)

    let isBusyActor = (actor: actor) => Belt_List.has(busyActorsIds, actor.id, (x, y) => x == y)
    List.filter(actor => !isBusyActor(actor), system.actors)
}
let isDeliveryInProcess = system => {
    let amountDelivered = system.queues
                                |> List.filter(queue => queue.final)
                                |> List.map(queue => queue.elements)
                                |> List.concat
                                |> List.length
    amountDelivered != system.amountToDeliver
}
let getElementFromQueue = (system, queueLocation: location) => {
    system.queues
        |> List.find(queue => queue.location == queueLocation)
        |> queue => queue.elements
        |> Belt_List.head
}
let removeElementFromQueue = (system, queueLocation: location) => {
    let updatedQueues = List.map(queue => {
            queue.location == queueLocation
                ? {...queue, elements: List.tl(queue.elements)}
                : queue
        }, system.queues);

    {
        ...system,
        queues: updatedQueues
    }
}
let addElementToQueue = (system, queueLocation, element) => {
    let updatedQueues = List.map(queue => {
            queue.location == queueLocation
                ? {...queue, elements: Belt_List.add(queue.elements, element)}
                : queue
        }, system.queues);

    {
        ...system,
        queues: updatedQueues
    }
}
let getUUID = (): uuid => string_of_int(Random.int(999999999))
let addActorEvent = (system, actor: actor) => {
    let elementFromFactory = getElementFromQueue(system, Factory);
    let elementFromPort = getElementFromQueue(system, Port)
    let actorLocation = actor.location;

    let (action, element, updatedSystem) = switch (actor.category) {
        | Car => switch (elementFromFactory, actorLocation) {
            | (Some({ destination: A }), Factory)
                => ({ activity: Move, start: Factory, finish: Port, duration: 1},
                    elementFromFactory,
                    removeElementFromQueue(system, Factory))
            | (_, Port)
                => ({activity: Move, start: Port, finish: Factory, duration: 1},
                    None,
                    system)
            | (Some({ destination: B }), Factory)
                => ({ activity: Move, start: Factory, finish: B, duration: 5},
                    elementFromFactory,
                    removeElementFromQueue(system, Factory))
            | (_, B)
                => ({activity: Move, start: B, finish: Factory, duration: 5},
                    None,
                    system)
            | (_, anyLocation)
                 => ({ activity: Wait, start: anyLocation, finish: anyLocation, duration: 1},
                    None,
                    system)
        };
        | Ship => switch (elementFromPort, actorLocation) {
            | (Some({ destination: A }), Port)
                => ({activity: Move, start: Port, finish: A, duration: 4},
                    elementFromPort,
                    removeElementFromQueue(system, Port))
            | (_, A)
                => ({ activity: Move, start: A, finish: Port, duration: 4},
                    None,
                    system)
            | (_, anyLocation)
                => ({ activity: Wait, start: anyLocation, finish: anyLocation, duration: 1},
                    None,
                    system)
        };
    }

    let event = {
        id: getUUID(),
        actorId: actor.id,
        payload: element,
        action: action,
        startTime: system.currentTime,
        endTime: None,
    };

    {
        ...updatedSystem,
        events: Belt_List.add(system.events, event)
    }
}
let generateNewEvents = system => List.fold_left(addActorEvent, system, getFreeActors(system))
let updateQueues = (event, system) => {
    switch (event.action.finish, event.payload) {
        | (location, Some(payload)) => addElementToQueue(system, location, payload)
        | _ => system
    }
}
let updateActor = (event: event, actor: actor) => {
    actor.id == event.actorId 
        ? {
            ...actor,
            location: event.action.finish,
        }
        : actor
}
let updateActors = (event, system) => {
    {
        ...system,
        actors: List.map(updateActor(event), system.actors)
    }
}
let processCompletedEvents = system => {
    let updateSystem = (system, event) => {
        system
        |> updateActors(event)
        |> updateQueues(event)
    };

    List.fold_left(updateSystem, system, getCompletedEvents(system))
}
let removeCompletedEvents = system => {
    let (eventsCompleted, eventsInProgress) = Belt_List.partition(system.events, isEventCompleted(system));
    
    {
        ...system,
        events: eventsInProgress,
        history:  eventsCompleted
                    |> List.map(event => {...event, endTime: Some(system.currentTime) })
                    |> Belt_List.concat(system.history),
    }
}
let updateTime = system => {...system, currentTime: system.currentTime + 1}
let tick = system => {
    system
    |> processCompletedEvents
    |> generateNewEvents
    |> removeCompletedEvents
    |> updateTime
}
let printSystem = system => {
    let convertToJson = event => {
        event
        |> encodeEvent
        |> Js.log
    };

    let eventsToPrint = system.history
        |> List.filter(event => event.action.activity != Wait)

    List.iter(convertToJson, eventsToPrint)
    Js.log("-----------------------------------------")
};

let solve = system => {
    let systemRef = ref(system);

    while (isDeliveryInProcess(systemRef^)) {
        systemRef := tick(systemRef^)
        printSystem(systemRef^)
    }

    Js.log("Result: " ++ string_of_int(systemRef^.currentTime - 1))
}
solve(system)