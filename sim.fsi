module Sim

type Ant =
    { 
        foodCarried: int
        xloc: int
        yloc: int
        directionVector: (int*int)*(int*int)
        lastvisitedpath: (int*int) list
        path: (int*int) list
    }

type TerritoryNode =
    { 
        food: int
        pheromone: int      
        numAnts: int
        isHome: bool 
    }

type PE =
    {
        antHomeX: int
        antHomeY: int
    }
    with
        static member initialize: unit -> PE

type Env =
    { 
        pe: PE
        simStep: int
        ants: seq<Ant>      
        phlocs: Map<int*int, bool>
        territory: Map<int*int, TerritoryNode> 
    }
    with
        static member initialize: p:PE -> Env

val territoryHeight: int
val territoryWidth: int
val maxFoodPerSquare: int

val DoOneStep: Env -> Env

