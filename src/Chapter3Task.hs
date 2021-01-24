{-# LANGUAGE InstanceSigs #-}

module Chapter3Task where

newtype Health = MkHealth {unHealth :: Integer} deriving (Show, Eq)

newtype Attack = MkAttack {unAttack :: Integer} deriving (Show, Eq)

newtype Defense = MkDefense {unDefense :: Integer} deriving (Show, Eq)

data KnightAction
  = KnightAttack 
  | DrinkPotion 
  | CastSpell 
  deriving (Show, Eq)

data MonsterAction
  = MonsterAttack
  | RunAway
  deriving (Show, Eq)

data Action
  = KnightAction KnightAction
  | MonsterAction MonsterAction
  deriving (Show, Eq)

data Knight = Knight {
  knightHealth :: Health,
  knightAttack :: Attack,
  knightDefense :: Defense,
  knightActions :: [Action] -- is there a way to limit to KnightActions only here? Can this lead to runtime errors?
} deriving (Show, Eq)

data Monster = Monster {
  monsterHealth :: Health,
  monsterAttack :: Attack,
  monsterActions :: [Action]
} deriving (Show, Eq)

class Player a where
  getHealth :: a -> Health
  getAttack :: a -> Attack
  getActions :: a -> [Action]
  getDefense :: a -> Defense
  setHealth :: a -> Health -> a
  setDefense :: a -> Defense -> a
  setActions :: a -> [Action] -> a

instance Player Knight where
    getHealth = knightHealth
    getAttack = knightAttack
    getActions = knightActions
    getDefense = knightDefense
    setHealth knight health = knight {knightHealth = health}
    setDefense knight defense = knight {knightDefense = defense}
    setActions knight actions = knight {knightActions = actions}

instance Player Monster where
    getHealth = monsterHealth
    getAttack = monsterAttack
    getActions = monsterActions
    getDefense _ = MkDefense 0
    setHealth monster _ = monster
    setDefense monster _ = monster -- should this be allowed?
    setActions monster actions = monster {monsterActions = actions}

attack :: (Player a, Player b) => a -> b -> (a, b)
attack attacker victim =
    (attacker, setHealth victim newHealth) where
        attack = unAttack $ getAttack attacker
        defense = unDefense $ getDefense victim
        health = unHealth $ getHealth victim
        newHealth = MkHealth $ health - (max 0 (attack + defense)) -- max 0 b/c one cannot gain health from defense
        

-- return Maybe here in case a player dies?
doAction :: (Player a, Player b) => Action -> a -> b -> (a, b)
doAction action attacker victim = -- maybe add @ constructor here?
    case action of
        KnightAction knightAction ->
            case knightAction of
                KnightAttack -> attack attacker victim
                DrinkPotion -> (setHealth attacker (MkHealth $ attackerHealth + 1), victim)
                CastSpell -> (setDefense attacker (MkDefense $ attackerDefense + 1), victim)
        MonsterAction monsterAction ->
            case monsterAction of
                MonsterAttack -> attack attacker victim
                RunAway -> (attacker, victim) -- how do we indicate that it terminated?
    where
        attackerHealth = unHealth $ getHealth attacker
        attackerDefense = unDefense $ getDefense attacker

fightRound :: (Player a, Player b) => a -> b -> (a, b)
fightRound playerA playerB = let
        cycleActions :: [Action] -> [Action]
        cycleActions actions = take (length actions) $ drop 1 $ cycle actions

        playerAnewActions = setActions playerA (cycleActions (getActions playerA))
        playerBnewActions = setActions playerB (cycleActions (getActions playerB))

        (playerAafterFight1, playerBafterFight1) = doAction (head $ getActions playerAnewActions) playerAnewActions playerBnewActions
        -- playerB2 can already be dead here? Should this return a just/maybe in this case?
        (playerBafterFights, playerAafterFights) = doAction (head $ getActions playerBafterFight1) playerBafterFight1 playerAafterFight1
    in
        (playerAafterFights, playerBafterFights)


-- can we reflect in datatypes that we need two disinct players?
-- maybe with return Either a b but I get the error "construct the infinite type: a ~ Either a b"
fight :: (Player a) => a -> a -> a
fight playerA playerB
    | playerAHealth < 0 = playerB
    | playerBHealth < 0 = playerA
    | otherwise = fight playerA1 playerB1
    where
        (playerA1, playerB1) = fightRound playerA playerB
        playerAHealth = unHealth $ getHealth playerA1
        playerBHealth = unHealth $ getHealth playerB1
