module Sim

open FSharp.Collections.ParallelSeq
open Primitives
open Graphics

// Main Idea and architecture from http://strangelights.com/blog/archive/2008/05/04/1613.aspx

// environment size
let territoryHeight = 80
let territoryWidth = 80

/// nest size
let antHomeDimension = 10

// ----------------------------------------------------
// food and decay
// ----------------------------------------------------

// typical 500 if many food nodes, else a higher value is good
let maxFoodPerSquare = 500

/// higher makes fewer food nodes, 100 is a decent wide distribution. 1000 is few
let chanceFoodPerSquare = 100

/// pheronomone decay step in percent per simulation step
let decayStepPercent = 7 // %
/// ph is set to 0 when lower than this value (is related to percent above of course so take care to have a number large enough to ensure it will be reset)
let pheromoneTreshold = 20

let foodPhFactor = 30
let maxPheronomone = 3600
let dropPheronomone foodCarried = foodCarried * foodPhFactor

// ----------------------------------------------------
// ant behavior
// ----------------------------------------------------
/// and hunt adventourness. percent chance of ant doing a totally random move when hunting even if ph or food is nearby.
let antAdventourness = 0 // when on ph trail ant will normally pop off and immediately start folloing trail again so not worth the effort to do adventourness really
/// ant max normal turn in 100th radians
let antMaxTurn = 110
/// ant will atempt to avoid visiting last N locs
let antVisitedLocMemLength = 5 
/// max food nodes carried by an ant 
let antMaxFoodCarried = 30

// for convenience we calculate and store a straight determined path for ants that are free wandering
// the length of the precalculated straight line is a random between the following values
let minPrecalcPathLength = 3
let maxPrecalcPathLength = 10
// ----------------------------------------------------

let envZeroVector = (0,0),(0,0)

// ----------------------------------------------------
// Main simulation types
// ----------------------------------------------------

/// TerritoryNode represents a single node in the ants territory
type TerritoryNode =
    { 
        food: int
        pheromone: int      
        numAnts: int
        isHome: bool 
    }
    with
        static member newNode home =
            { food = 0;
              pheromone = 0;
              numAnts = if home then 1 else 0;
              isHome = home }
        static member newNodeFood f home =
            { food = if home then 0 else f;
              pheromone = 0;
              numAnts = if home then 1 else 0;
              isHome = home }
        member n.hasFood = n.food > 0
        member n.hasAnt = n.numAnts > 0
        member n.antArrive () = { n with numAnts = n.numAnts + 1 }
        member n.antLeave () = { n with numAnts = n.numAnts - 1 }

/// Ant represents an ant that moves within the territory
type Ant =
    { 
        foodCarried: int
        xloc: int
        yloc: int
        directionVector: (int*int)*(int*int)
        lastvisitedpath: (int*int) list
        path: (int*int) list // pre calculated path to follow
    }
    with
        /// creates a new instance of an ant with the default values
        static member newAnt (x,y) = {xloc = x; yloc = y; directionVector = envZeroVector; foodCarried = 0; lastvisitedpath=[]; path = [] }

/// Environment parameters
type PE =
    {
        antHomeX: int
        antHomeY: int
    }
    with
        static member initialize() =
            let x = 
                match random.Next(3) with
                | 0 -> 0
                | 1 -> territoryWidth/2 - antHomeDimension/2
                | 2 -> territoryWidth - antHomeDimension
                | _ -> failwith ""

            let y = 
                match random.Next(3) with
                | 0 -> 0
                | 1 -> territoryHeight/2 - antHomeDimension/2
                | 2 -> territoryHeight - antHomeDimension
                | _ -> failwith ""

            {antHomeX = x; antHomeY = y}

        member m.antHomeXCenter = m.antHomeX + antHomeDimension/2
        member m.antHomeYCenter = m.antHomeY + antHomeDimension/2


/// Env - The environment - represents both the ants and the nodes they move within
type Env =
    { 
        pe: PE
        simStep: int
        ants: seq<Ant>      
        phlocs: Map<int*int, bool> // locations having a phvalue>0
        territory: Map<int*int, TerritoryNode> 
    }
    with
        /// initalize the environment with the default values
        static member initialize(p:PE) =
            // create a map of points to node values
            let terr = 
                let createNode x y =
                   let randomFoodDropValue = random.Next( chanceFoodPerSquare )
                   let home = x >= p.antHomeX && x < p.antHomeX + antHomeDimension && y >= p.antHomeY && y < p.antHomeY + antHomeDimension
                   if randomFoodDropValue = 0 then
                        TerritoryNode.newNodeFood ( random.Next( maxFoodPerSquare ) ) home
                   else
                        TerritoryNode.newNode home

                let points =
                    seq { for x in [0 .. territoryWidth - 1] do
                            for y in [0 .. territoryHeight - 1] do
                              yield x,y }

                Seq.fold (fun acc (x,y) -> Map.add (x,y) (createNode x y) acc) Map.empty points
            
            let antSequence =
                seq { for x in [p.antHomeX .. p.antHomeX + antHomeDimension - 1] do
                        for y in [p.antHomeY .. p.antHomeY + antHomeDimension - 1] do
                            yield Ant.newAnt (x,y)
                    }
                                      
            { pe=p; simStep = 0; ants = antSequence; phlocs = Map.empty; territory = terr }
       
        member m.IncrementSimStep = m.simStep + 1
        
        member m.replaceNode loc newNode =
            if m.territory.ContainsKey(loc) then
                let newTerr = m.territory.Remove(loc)
                { m with  territory = newTerr.Add(loc, newNode) }
            else
                { m with  territory = m.territory.Add(loc, newNode) }
        
        member m.PhLocClear loc = { m with phlocs = m.phlocs.Remove(loc) }
        member m.PhLocSet loc = 
            if not (m.phlocs.ContainsKey(loc)) then 
                { m with phlocs = m.phlocs.Add(loc, true) }
            else
                m
        
        member m.FoodLeft = 
            let envFood = 
                seq {
                        for nodeMap in m.territory do
                            if nodeMap.Value.food > 0 && not m.territory.[nodeMap.Key].isHome then
                                yield (nodeMap.Value.food)
                    }
            
            envFood |> PSeq.sum 

// ----------------------------------------------------
// Simulation helpers
// ----------------------------------------------------


/// make an ant drop some food
let TNDropFood node ant =
    if (node.food = maxFoodPerSquare) then
        false, node, ant
    else
        let space = maxFoodPerSquare - node.food
        if (space < ant.foodCarried) then
            true, { node with food = node.food + space },  { ant with foodCarried = ant.foodCarried - space }
        else
            true, { node with food = node.food + ant.foodCarried }, { ant with foodCarried = 0; path = []; directionVector = envZeroVector }
 
/// remove the appropriate amount of food from a node
let TNGetFood (node:TerritoryNode) f =
    if node.hasFood then
        let food = node.food
        if food >= f then
            { node with food = food - f },
            f                            
        else
            let retFood = node.food
            { node with food = 0 },
            retFood
    else node, 0
     
/// move ant to a new node. NB the new node must be inside the environment.
let ANTGoToNode env ant (x, y) =
    let currLoc = (ant.xloc,ant.yloc)
    let newLoc = (x,y)
    let currNode = env.territory.[currLoc]
    let newNode = env.territory.[newLoc]
    let newlastvisitedpath =
        if newNode.isHome then [] else (x,y) :: take antVisitedLocMemLength ant.lastvisitedpath
    let newPath = listtail ant.path
    let newDirectionVector = if List.length ant.path = 0 then envZeroVector else ant.directionVector
    let env = env.replaceNode currLoc (currNode.antLeave())
    let env = env.replaceNode newLoc (newNode.antArrive())

    /// ant spray pheromone on current loc
    let SprayPh env ant (x,y) =
        let currNode = env.territory.[(x,y)]
        let envPhDrop = (env.replaceNode (x,y) ({currNode with pheromone = currNode.pheromone + dropPheronomone ant.foodCarried }))        
        envPhDrop.PhLocSet (x,y)

    SprayPh env ant (x,y), { ant with xloc = x; yloc = y; lastvisitedpath = newlastvisitedpath; path = newPath; directionVector = newDirectionVector }

/// test if coordinate is within the territory
let GTIsLocReal (x, y) =
    x >= 0 && x < territoryWidth && y >= 0 && y < territoryHeight

/// get any random location around (x,y) Can be outside the territory
let getRndLoc (x,y) = 
    let rnd = random.Next( 8 )
    match rnd with
    | 0 -> (x, y+1)
    | 1 -> (x, y-1)
    | 2 -> (x+1, y)
    | 3 -> (x+1, y+1)
    | 4 -> (x+1, y-1)
    | 5 -> (x-1, y)
    | 6 -> (x-1, y+1)
    | 7 -> (x-1, y-1)
    | _ -> failwith ""

/// get a random location around (x,y) inside territory
let rec getRealRndLoc (x,y) =     
    let testLoc = getRndLoc (x,y)
    if GTIsLocReal testLoc then
        testLoc
    else
        getRealRndLoc (x,y)

/// get a random location around (x,y) inside nest
let rec getNestRndLoc env (x,y) =     
    let testLoc = getRndLoc (x,y)
    if GTIsLocReal testLoc && env.territory.[testLoc].isHome then
        testLoc
    else
        getNestRndLoc env (x,y)

/// get a random location around (x,y) outside nest
let rec getOutsideNestRndLoc env (x,y) =     
    let testLoc = getRndLoc (x,y)
    if GTIsLocReal testLoc && not env.territory.[testLoc].isHome then
        testLoc
    else
        getOutsideNestRndLoc env (x,y)

/// move ant to random node inside nest
let ANTMoveRandomlyInsideNest env ant = 
    ANTGoToNode env {ant with path = []; directionVector = envZeroVector } (getNestRndLoc env (ant.xloc, ant.yloc))

/// move ant to random node outside nest
let ANTMoveRandomlyOutsideNest env ant = 
    ANTGoToNode env {ant with path = []; directionVector = envZeroVector } (getOutsideNestRndLoc env (ant.xloc, ant.yloc))

/// move ant one step totally random
let ANTMoveRandomly env ant = 
    ANTGoToNode env {ant with path = []; directionVector = envZeroVector } (getRealRndLoc (ant.xloc, ant.yloc))

/// move ant one step along precalculated path
let ANTMoveFollowDeterminedPath env ant =
    if GTIsLocReal (List.head ant.path) then
        ANTGoToNode env ant (List.head ant.path)
    else
        ANTMoveRandomly env ant

/// get random node within whole territory different from x1,y1
let rec getRandomEndNodeOutsideNest env ant =
    let (x1,y1) = (ant.xloc, ant.yloc)
    let (x2,y2) = random.Next (territoryWidth-1), random.Next (territoryHeight-1)
    if (x1,y1) <> (x2,y2) && not env.territory.[(x2,y2)].isHome then
        (x2,y2)
    else
        getRandomEndNodeOutsideNest env ant

/// get random nearby node within the precalculated max length distance we use
let rec getRandomNearbyEndNodeOutsideNest env ant = 
    let (x1,y1) = (ant.xloc, ant.yloc)
    let (x2,y2) = random.Next (x1-maxPrecalcPathLength-1, x1+maxPrecalcPathLength+2), random.Next (y1-maxPrecalcPathLength-1, y1+maxPrecalcPathLength+2)
    if (x1,y1) <> (x2,y2) && GTIsLocReal(x2,y2) && not env.territory.[(x2,y2)].isHome then
        (x2,y2)
    else
        getRandomNearbyEndNodeOutsideNest env ant

/// get line between two points, remove first point and truncate to max n length
let getShortestPath maxlen (x1,y1) (x2,y2) = 
    take maxlen (listtail (vectorToLine (x1,y1) (x2,y2)))

/// move out of nest
let ANTMoveOutOfNest env ant =     
    if List.length ant.path = 0 then
        let newPath = getShortestPath antHomeDimension (ant.xloc, ant.yloc) (getRandomEndNodeOutsideNest env ant)
        let newDirectionVector = newPath.Head, newPath.Item(newPath.Length-1)
        ANTMoveFollowDeterminedPath env { ant with path=newPath; directionVector=newDirectionVector }
    else
        ANTMoveFollowDeterminedPath env ant

// ----------------------------------------------------
/// ant hunt
let ANTMoveHunting env ant =

    /// calculate and decide what neighbouring location is the best move for an ant, return (-1,-1) if unable to decide    
    let ANTDecideNextMove env ant = 

        /// get a weight for attractiveness of a location.
        let getUrgeVal env ant (x,y) =
            /// true if ant has visited (x,y) in N last moves (it does not need a long memory for this to work satisfactory)
            let hasVisitedLocRecently ant (x,y) =
                Seq.exists (fun (x1,y1) -> (x,y) = (x1, y1)) ant.lastvisitedpath

            /// higher weight for direction away from nest
            let homeDirectionWeight (x1,y1) (x2,y2) =
                let a = (sign (x2-x1) = sign (env.pe.antHomeX-x1)) || (x2 >= env.pe.antHomeX && x2 < env.pe.antHomeX + antHomeDimension )
                let b = (sign (y2-y1) = sign (env.pe.antHomeY-y1)) || (y2 >= env.pe.antHomeY && y2 < env.pe.antHomeY + antHomeDimension )
                match a, b with
                | _ when a && b -> -1
                | _ when a || b -> 1
                | _ -> 10

            if GTIsLocReal (x,y) && not env.territory.[(x,y)].isHome && not (hasVisitedLocRecently ant (x,y)) then
                let w = env.territory.[(x,y)].pheromone + env.territory.[(x,y)].food * 1000
                let hw = homeDirectionWeight (ant.xloc, ant.yloc) (x,y)
                if hw = -1 then -1 else w + hw*w
            else -1

        // check attractiveness of all neighbouring locations
        let x,y = ant.xloc, ant.yloc
        let neighbours =
            [
            (x, y+1), getUrgeVal env ant (x, y+1) ;
            (x, y-1), getUrgeVal env ant (x, y-1) ;
            (x+1, y), getUrgeVal env ant (x+1, y) ;
            (x+1, y+1), getUrgeVal env ant (x+1, y+1) ;
            (x+1, y-1), getUrgeVal env ant (x+1, y-1) ;
            (x-1, y), getUrgeVal env ant (x-1, y) ;
            (x-1, y+1), getUrgeVal env ant (x-1, y+1) ;
            (x-1, y-1), getUrgeVal env ant (x-1, y-1)
            ]    
        
        let sumPh = List.sumBy (fun (ph) -> snd ph) neighbours

        let randomMove = 
            if antAdventourness > 0 && antAdventourness <= 100 then
                random.Next (100 / antAdventourness) = 0
            else
                false
        if randomMove then
            getRealRndLoc (ant.xloc, ant.yloc)
        else
            if sumPh <= 0 then
                (-1,-1) // didnt find any worthwile to visit
            else
                // pick the best in weighted list
                let possibleDirs = List.filter (fun ph -> (snd ph) > 0) neighbours
                let orderedPh = List.rev (List.sortBy (fun (ph) -> snd ph) possibleDirs)
                fst orderedPh.Head

    /// get a new precalculated hunt path or use existing
    let getHuntPath env ant =    

        let getNewHuntPath env ant =        
            let slightTurn ant =
                let newx, newy = 
                    let radians = 
                        let dx = fst (snd ant.directionVector) - fst (fst ant.directionVector)
                        let dy = snd (snd ant.directionVector) - snd (fst ant.directionVector)                
                        let randomturn = 
                            let rnd = (double (random.Next (antMaxTurn))) / 100.0
                            if random.Next(2) = 0 then rnd else -rnd
                        atan2 (double dy) (double dx) + randomturn
                    (cos radians * 25.0), (sin radians * 25.0)
                ant.xloc + (int (round newx)), ant.yloc + (int (round newy))

            let newPath = 
                let rndlength = random.Next (minPrecalcPathLength, maxPrecalcPathLength)
                if ant.directionVector = envZeroVector || (List.length (ant.path) > 0 && not (GTIsLocReal (List.head (ant.path)))) then
                    getShortestPath rndlength (ant.xloc, ant.yloc) (getRandomNearbyEndNodeOutsideNest env ant)
                else
                    let newEndPoint = slightTurn ant
                    getShortestPath rndlength (ant.xloc, ant.yloc) newEndPoint
            let newDirectionVector = newPath.Head, newPath.Item(newPath.Length-1)
            newPath, newDirectionVector

        if List.length (ant.path) = 0 || not (GTIsLocReal (List.head (ant.path))) || env.territory.[ant.path.Head].isHome then
            getNewHuntPath env ant
        else
            ant.path, ant.directionVector

    // ant hunt main:
    let nextMove = ANTDecideNextMove env ant
    if nextMove <> (-1,-1) then
        ANTGoToNode env {ant with path = []; directionVector = envZeroVector} nextMove
    else
        // Ant is unable to find a good trait nearby, so follow predetermined path instead
        let newPath, newDirectionVector = getHuntPath env ant
        if GTIsLocReal newPath.Head && env.territory.[newPath.Head].isHome then
            ANTMoveRandomlyOutsideNest env ant
        else
            ANTMoveFollowDeterminedPath env {ant with path = newPath; directionVector = newDirectionVector}


// ----------------------------------------------------
/// get a list of points that is the shortest direct line home
let getShortestPathHome env (x,y) = 
    listtail (vectorToLine (x,y) (env.pe.antHomeXCenter, env.pe.antHomeYCenter))

// ----------------------------------------------------
/// main encoding of ants behavior
let ANTBehave env ant =
    let (x,y) = (ant.xloc, ant.yloc)
    let currNode = env.territory.[(x,y)]
    if ant.foodCarried > 0 then
        // ant carries food
        if currNode.isHome then
            // attempt to drop food or move to another node if no space to drop
            let isDropped, cNode, ant = TNDropFood currNode ant
            let env = env.replaceNode (x, y) cNode
            if not isDropped then
              ANTMoveRandomlyInsideNest env ant
            else
              env, ant
        else
            ANTMoveFollowDeterminedPath env ant
    else 
        // ant does not carry food
        if currNode.isHome then
            ANTMoveOutOfNest env ant
        else
            if currNode.hasFood then
                // get food (dont move)
                let tillMaxFood = antMaxFoodCarried - ant.foodCarried
                let cNode,foodGot = (TNGetFood currNode tillMaxFood)
                let newPath = getShortestPathHome env (x,y)
                let newDirectionVector = newPath.Head, newPath.Item(newPath.Length-1)
                env.replaceNode (x,y) cNode, { ant with foodCarried = ant.foodCarried + foodGot; path = newPath; directionVector = newDirectionVector}
            else
                ANTMoveHunting env ant

// ----------------------------------------------------

let DecayPh (env: Env) (x,y) =
    let nodeDecay node =
        let decayStep = node.pheromone * decayStepPercent / 100
        let afterdecayed = if decayStep > node.pheromone || node.pheromone - decayStep < pheromoneTreshold then 0 else node.pheromone - decayStep
        let newPh = doNotExceedMax afterdecayed maxPheronomone
        if newPh > 0 then
            {node with pheromone = newPh}
        else
            {node with pheromone = 0}

    env.replaceNode (x, y) (nodeDecay env.territory.[(x,y)])

/// Update Pheromones values in the whole environment
let UpdateEnvPheromones env =    

    let phDecayOneLoc env (x,y) =
        let env = DecayPh env (x,y)
        if env.territory.[(x,y)].pheromone = 0 then
            env.PhLocClear (x,y)
        else
            env

    Map.toSeq(env.phlocs) |> PSeq.fold (fun env ((x,y),b) -> phDecayOneLoc env (x,y)) env

// ----------------------------------------------------                        
/// Advance one step in simulation
let DoOneStep env = 
        
    let doAntBehaveOneStep (env, acc) ant =
        let envNew, antNew = ANTBehave env ant
        envNew, antNew:: acc
                  
    let env, newAnts = PSeq.fold (fun env ant -> doAntBehaveOneStep env ant) (env, []) env.ants
        
    let env = UpdateEnvPheromones env

    if env.simStep % 100 = 0 && env.FoodLeft = 0 then
        Env.initialize (PE.initialize()) // auto restart
    else
        { env with simStep = env.IncrementSimStep; ants = Seq.ofList newAnts }

