; Agent-based Model of bees foraging on a meadow with two co-flowering plant species competing for the shared pollinator

; Masterthesis of Helen Czioska
; Faculty of Environment & Natural Resources at the Albert-Ludwigs-University Freiburg


breed [bees bee]
bees-own [ 
  reward-memory   ;string - momory for the amount of reward gained from each of the last 4 visits
  species-memory  ;string - remembers the species of the last X flowers
  flower-memory   ;string - memory of the location of the last 4 visited floweres
  array           ;array - vision of the bee-agent on the grid
  choice          ;boolean - gives the currently prefferred flower (flower-constancy); changes with unsuccsessfull search and bad reward
  change-prob     ;float - propability to change the preferred flower type. Increases with low reward and unsuccsessfull search
  flowers-visited ;integer - counter how many flowers this bee-agent already visited
  flight-count    ;integer - duration of unsuccessfull search for the next preferred flower
  change-count1   ;integer - how often does this pollinator switch preferences due to bad reward?
  change-count2   ;integer - how often does this pollinator switch preferences due to  unsuccsessfull search?
  handling-time   ;integer - time the bee-agent needs to extract all reward (handling time = reward * 4 + 0.5 + penalty)
  change-penalty  ;integer - bee-agent needs additional 3seconds to extract all reward when unexperienced
  average-reward-count ; Integer - counter for foraging on flowers with average reward
  ]

patches-own [ 
  species       ;boolean - defined species, does not change
  reward        ;float - reward (max 1J), emptied with every visit by a bee-agent and restored by the reward-function
  visit-count   ;integer - counter how often this flower was visited
  pollination-count ;integer - counter for every successfull pollination
  ]

globals[
  flower-number  ;integer - how many flowers are on the meadow
  flower-number1 ;integer - how many flowers of species 1 are on the meadow
  flower-number2 ;integer - how many flowers of species 2 are on the meadow
  cluster-number1  ;integer - number of clusters of species 1
  cluster-number2  ;integer - number of clusters of species 2
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup 
 clear-all


; create a meadow with patches of flowers of two pollinator-dependent flower species


 ask patches [set pcolor 64 set species 0] ;the grass
 
 ;create n patches with an average of cluster-degree flowers
 set flower-number round ((flower-cover / 100) * (count patches))
 set flower-number1 round (flower-number * (frequency / 100))
 set flower-number2 round (flower-number - flower-number1)
 set cluster-number1 (flower-number1 / cluster-degree)
 set cluster-number2 (flower-number2 / cluster-degree)
 
 if(flower-number1 > 0) 
   [  ifelse (cluster-number1 >= 1) 
     [ask n-of cluster-number1 patches with [species = 0] [set species 1 set pcolor 47 set reward 1 ]]
     [ask n-of 1 patches with [pcolor = green] [set species 1 set pcolor 47 set reward 1 ]]
   ]
   
 if(flower-number2 > 0) 
  [
 ifelse (cluster-number2 >= 1)
    [ask n-of cluster-number2 patches with [species = 0] [set species 2 set pcolor 104 set reward 1 ]]
    [ask n-of 1 patches with [pcolor = green] [set species 2 set pcolor 104 set reward 1 ]]
  ]

 while [count patches with [species > 0] < flower-number ]
 [ask one-of patches with [species > 0] 
   [
    if (species = 1 and (count patches with [species = 1] < flower-number1))
      [ifelse (any? patches with [species = 1 and any? neighbors with [species = 0]])
        [if(any? neighbors with [species = 0])[ask one-of neighbors with [species = 0] [set species [species] of myself set pcolor [pcolor] of myself set reward 1 ]]]  
        [ask n-of 1 patches with [pcolor = green] [set species 1 set pcolor 47 set reward 1 ]]
      ]
   
   if (species = 2 and (count patches with [species = 2] < flower-number2))
      [ifelse (any? patches with [species = 2 and any? neighbors with [species = 0]])
        [if(any? neighbors with [species = 0]) [ask one-of neighbors with [species = 0] [set species [species] of myself set pcolor [pcolor] of myself set reward 1 ]]]
        [ask n-of 1 patches with [pcolor = green] [set species 2 set pcolor 104 set reward 1 ]] 
      ]
   ]
 ]
 
 
 ; create the bee-agents
 
  create-bees number-bees 
  [ set shape "bee" ;Agents with an actual bee-shape :)
    ; set shape "circle" set color 9.9 ; Agents with a simple circle, faster in calculations
    setxy random-xcor random-ycor 
    set reward-memory (list)
    set flower-memory (list)
    set species-memory (list)
    set flight-count 0
    set flowers-visited 0
    set handling-time 0
    set change-penalty 3
    let next-flower min-one-of patches with [species > 0 ][distance myself]
    face next-flower set choice [species] of next-flower 
    ;bee-agents choose the closest flower to their starting point for initial preference
    ]

  reset-ticks
  
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  
; bee-agents can either search for a flower or forage on one. The handling time is reward-dependent and needs more than one tick  
ask bees [
      ifelse (handling-time > 0) 
           [stay]           
           [ifelse (([choice] of self = species) and (not member? patch-here [flower-memory] of self)) 
             [visit]    
             [move]  
           ]    
]

ask patches [if (reward < 1) [set reward reward + reward-function]] ; see Kunin & Iwasa 1996: Max. 1 J

tick

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to move

     ifelse (flight-count < flightsteps-until-change)

              [
                ; find out if there are any flowers in the visible area of the bee which have the preferres species AND were not visited before
                set array patches in-cone view 180 with [(species = [choice] of myself) and (not member? self [flower-memory] of myself) ] 
                let next-flower min-one-of array [distance myself]
                ifelse any? array 
                    [ face next-flower fd 1] ;face the nearest flower and move to it in a straight line (see Viswanathan 2008)
                    [ rt random-normal 0 stdev-angle fd 1 set flight-count flight-count + 1 ]
              ]
   
   
              [
                set change-prob change-prob + 0.1 ; every second the bee-agent does not find a preferred flower, the change-probability increases by 10% (see Chittke et al. 1997)
                ifelse (random-float 1 < change-prob) ; if the random-number is below the change-prob, the bee-agent just picks the next available flower regardless of its species
                [  
                  set array patches in-cone view 180 with [ (species > 0) and (not member? self [flower-memory] of myself) ] 
                  let next-flower min-one-of array [distance myself]
                  ifelse any? array
                    [ face next-flower fd 1 set choice [species] of next-flower set change-count2 change-count2 + 1 set change-prob 0]
                    [ rt random-normal 0 stdev-angle fd 1 set flight-count flight-count + 1 ]
                ]
                [ rt random-normal 0 stdev-angle fd 1 set flight-count flight-count + 1 ]
              ]
  
end           
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to visit

   if (length (reward-memory) > 0) ; precaution for the first visit
   [
      if (reward < (mean reward-memory) / 2) [set change-prob change-prob + 0.1 set average-reward-count 0] ;bad reward: change-porpability increases
      if (reward > (mean reward-memory) * 2) [set change-prob 0 set average-reward-count 0] ;very good reward: change-porpability decreases
      if ((reward < (mean reward-memory) * 2) and (reward > (mean reward-memory) / 2)) [ set average-reward-count average-reward-count + 1]
      if (average-reward-count > 5) [set change-prob 0 set average-reward-count 0] ; 5 times average reward: change-porpability decreases
      
      ifelse (choice = 1)
           [ if (random-float 1 < change-prob) [set choice 2 set change-prob 0 set change-count1 change-count1 + 1 set average-reward-count 0]]
           [ if (random-float 1 < change-prob) [set choice 1 set change-prob 0 set change-count1 change-count1 + 1 set average-reward-count 0]]
   ]
   
   ;remember reward
   while [(length reward-memory ) >= 4] [ set reward-memory  (but-last reward-memory ) ]
   set reward-memory  (fput [reward] of patch-here reward-memory  )
   
   ;remember flower-location
      ; Every pollinator has a memory for visited flowers. Pollinators avoid recently visited flowers (see Goulson 1999 for review)
      ; Goulson 2000: "Pollinators can remember the location of the last 4 visited flowers"
   while [(length flower-memory) >= 4] [ set flower-memory (but-last flower-memory) ]
   set flower-memory (fput patch-here flower-memory )
   
   
   ;calculate handling-time
      ; 4 s/J (Roubik 1989 in Kunin & Iwasa 1996)
      ; 0.5s constant minimum handling time (see Kunin & Iwasa 1996)
      ; 3s Penalty for change of flower, "experience extract time" (see Kunin & Iwasa 1996)
      
   if (length (species-memory) > 0) 
   [ifelse (first species-memory = [species] of patch-here) 
     [set change-penalty 0]
     [set change-penalty 3]
   ]
   set handling-time (round (reward * 4 + 0.5 + change-penalty))
          
   if ( member? ([species] of patch-here) species-memory)
   [set pollination-count pollination-count + 1]
   
   while [(length species-memory) >= pollen-reach] [ set species-memory (but-last species-memory) ]
   set species-memory (fput [species] of patch-here species-memory )
   
   set flight-count 0  
   set flowers-visited flowers-visited + 1
   set reward 0                                 
   set visit-count visit-count + 1
   
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to stay
  
set handling-time handling-time - 1

end  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report visits [spec]
report (sum [visit-count] of patches with [species = spec])
end

to-report pollination-success [spec]
report (sum [pollination-count] of patches with [species = spec])
end

to-report reward-report [spec]
report ( mean [reward] of patches with [species = spec])
end

to-report count-change-flightsteps
report sum [change-count2] of bees
end

to-report count-change-reward
report sum [change-count1] of bees
end

to-report number-flowers [spec]
report count patches with [species = spec]
end
@#$#@#$#@
GRAPHICS-WINDOW
460
14
1187
762
-1
-1
7.17
1
10
1
1
1
0
1
1
1
0
99
0
99
0
0
0
ticks
30.0

SLIDER
199
209
371
242
number-bees
number-bees
0
50
10
5
1
NIL
HORIZONTAL

SLIDER
199
103
371
136
flower-cover
flower-cover
0
100
20
5
1
NIL
HORIZONTAL

BUTTON
19
22
82
55
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
127
23
190
56
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
199
139
371
172
frequency
frequency
0
100
25
1
1
NIL
HORIZONTAL

BUTTON
194
23
257
56
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
17
103
189
136
stdev-angle
stdev-angle
0
90
30
1
1
NIL
HORIZONTAL

SLIDER
17
139
190
172
flightsteps-until-change
flightsteps-until-change
0
100
5
1
1
NIL
HORIZONTAL

INPUTBOX
18
246
189
306
reward-function
4.0E-4
1
0
Number

SLIDER
199
174
371
207
cluster-degree
cluster-degree
1
100
5
1
1
NIL
HORIZONTAL

SLIDER
18
174
190
207
pollen-reach
pollen-reach
1
16
1
1
1
NIL
HORIZONTAL

SLIDER
18
209
190
242
view
view
0
50
6
1
1
NIL
HORIZONTAL

TEXTBOX
18
79
192
98
Behaviour Variables:
15
0.0
1

TEXTBOX
202
79
352
98
Meadow Variables:
15
0.0
1

TEXTBOX
22
309
190
351
Default values are based on empirical research and should be changed with care. 
11
0.0
1

MONITOR
16
408
176
453
# Flowers Species 1 (yellow)
flower-number1
1
1
11

MONITOR
178
408
337
453
# Flowers Species 2 (blue)
flower-number2
17
1
11

PLOT
16
457
216
607
Vists per Species
Ticks
Visitation
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"1" 1.0 0 -955883 true "" "plot visits 1"
"2" 1.0 0 -13345367 true "" "plot visits 2"

PLOT
17
609
217
759
Pollination
Ticks
Pollination
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"1" 1.0 0 -955883 true "" "plot pollination-success 1"
"2" 1.0 0 -13345367 true "" "plot pollination-success 2"

PLOT
219
457
419
607
Visits per Flower
Ticks
Per-Flower VR
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"1" 1.0 0 -955883 true "" "plot visits 1 / flower-number1"
"2" 1.0 0 -13345367 true "" "plot visits 2 / flower-number2"

PLOT
219
609
419
759
Pollination per Flower
Ticks
Per-Flower Poll.
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"1" 1.0 0 -955883 true "" "plot pollination-success 1 / flower-number1"
"2" 1.0 0 -13345367 true "" "plot pollination-success 2 / flower-number2"

@#$#@#$#@
## WHAT IS IT?

Agent-based Model of bees foraging on a meadow with two co-flowering plant species competing over pollinating service. The model was developed on empirical findings for foraging rules and pollinator behavior.
Its aim is to show the change of visitation rate and pollination success for each species with changing floral cover, frequency and degree of clustering. 


This model was created for the Master Thesis of Helen Czioksa
submitted to the Faculty of Environment & Natural Resources
at the Albert-Ludwigs-University Freiburg


## HOW IT WORKS

### Assumptions

In the model, all pollinators are identical and the two flower types only differ in their initial species identity. Reward regrowth, handling times to extract the reward and its attractiveness towards the pollinators is identical for both species. Corolla color is only assigned for better visualization and is not important for the model or the pollinators, respectively.The pollinators behave under the theory of flower constancy which is empirically tested and proven for various pollinators (e.g. Hill et al. 1997 for hones bees, Chittka et al. 1997 for bumble bees). Flower constancy is the tendency of a pollinator to keep visiting the same flower species instead switching to more rewarding or closer species (e.g. Chittka et al. 1999). In the ABM, all pollinators forage exclusively on the currently preferred species until the species is either not rewarding any more or the search for a next flower is unsuccessful. 

### Model Environment

In the model, the "meadow" has 100x100grid cells with horizontally and vertically wrapping to avoid edge effects. Every grid cell can either contain a single flower of one of the two species or grass. The flowers of those two species are randomly distributed over the meadow. Every flower contains 1 Joule of floral reward in the beginning of each simulation run. The pollinators ("bee-agents") are also randomly distributed over the modeling environment, no hive is assigned (no central place foragers). Bee-agents start without a fixed preference for a flower type but just pick the closest one when the simulation starts. The energetic costs and the limit of gained rewards of the pollinators are ignored. Furthermore, they do not communicate and always empty a flower completely. 

### Behaviour Rules

As mentioned in the assumptions, the behavior of the bee-agents is strongly influences by the theory of flower constancy (e.g. Goulson 1999). Bee-agents are always in favor of one of the two flowering species and forage exclusively on this species. The preference can change due to lack of searching success and a series of low rewards of the preferred flower. Pollinators avoid recently visited flowers. Every bee-agent is equipped with a memory to remember the location of the last four already visited flowers. The bee-agent can either search for a flower or visit one. If there is any preferred and unvisited flower in sight, the searching bee-agent moves on direct way towards the flower, otherwise it continues searching. 
Every bee-agent can detect flowers from a distance of 0.7m with an equivalent of 6 grid cells. The vision is reduced to a 180° cone-shaped field to the front of the agent. Pollinators tend to keep their direction while foraging. In the model, I used a correlated random walk (CRW) to achieve a relatively natural movement. If the bee-agent searches for 5 seconds (= 5 ticks) without finding any preferred and unvisited flower, the likelihood of changing its preference increases by 10\% with every additional tick.

When a bee-agent encounters a preferred and unvisited flower it takes up all its reward. The maximal reward a flower can contain is 1 Joule and refills each tick by a linear function ("reward-function"). The handling time involves three components: a time proportional to the amount of taken reward, a reward-independent constant and a skill factor (Kunin and Iwasa 1996). In my model, a bee-agent requires 4 seconds to extract one Joule of reward plus a reward-independent handling time of 0.5 seconds. When the bee-agent just changed its flower preference it gets a 3 second penalty for inexperience (Roubik1992 in Kunin and Iwasa 1996).

The reward taken is stored in the agent-own reward-memory. Every agent can remember the last four receives rewards. When visiting a flower, the bee-agent compares this memory with the current reward quantity. If the reward is less than half the average in the memory, the likelihood to abandon flower constancy and visit another species next increases by 10\%. If the reward is exceptionally good (double of there remembered average), the change probability is set to zero  (Chittka et al. 1997).

After reward-collection is completed, the bee-agent updates its flower-memory and its reward-memory and continues foraging. Each visit and successful pollination is recorded for later analysis. 

## HOW TO USE IT

The use of the model is fairly simple. "Setup" produces a meadow with the desired number of flowers, clusters and bees. Every tick represents a second. You can observe bees foraging on the flowers and report the visitation rate and the number of successfull pollinations for each species.

## THINGS TO TRY

Interesting simulation options include the range of all sliders in the user interface tab. The parameters are divided into two groups: Behavioral variables and meadow variables. The default values of behavioral variables are empirically based and should be changed with care. The meadow variables can be set to any desired value. 

## EXTENDING THE MODEL

An interesting development area is the implementation of a meta level with several meadows differing in their variables neighboring each other. Bee-agents could swith between those patches on a given cost. 

Another approach would be the integration of more flower species, maybe even with a difference in attractiveness or reward function. 


## CREDITS AND REFERENCES

Chittka, L., Gumbert, A., and Kunze, J. (1997). Foraging dynamics of bumble bees: correlates of movements within and between plant species. Behavioral Ecology, 8(3):239–249.

Chittka, L., Tomson, J. D., and Waser, N. M. (1999). Flower constancy, insect psychology, and plant evolution. Naturwissenschaen, 86(8):361–377.

Goulson, D. (1999). Foraging strategies of insects for gathering nectar and pollen, and implications for plant ecology and evolution. Perspectives in Plant Ecology, Evolution and Systematics, 2(2):185–209.

HILL, P. S., WELLS, P. H., and WELLS, H. (1997). Spontaneous fower constancy and learning in honey beesas a function of colour. Animal Behaviour, 54(3):615–627.

Kunin, W. and Iwasa, Y. (1996). Pollinator foraging strategies in mixed foral arrays: density effects and foral constancy. eoretical population biology, 49(2):232–263.


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bee
true
0
Polygon -1184463 true false 152 149 77 163 67 195 67 211 74 234 85 252 100 264 116 276 134 286 151 300 167 285 182 278 206 260 220 242 226 218 226 195 222 166
Polygon -16777216 true false 150 149 128 151 114 151 98 145 80 122 80 103 81 83 95 67 117 58 141 54 151 53 177 55 195 66 207 82 211 94 211 116 204 139 189 149 171 152
Polygon -7500403 true true 151 54 119 59 96 60 81 50 78 39 87 25 103 18 115 23 121 13 150 1 180 14 189 23 197 17 210 19 222 30 222 44 212 57 192 58
Polygon -16777216 true false 70 185 74 171 223 172 224 186
Polygon -16777216 true false 67 211 71 226 224 226 225 211 67 211
Polygon -16777216 true false 91 257 106 269 195 269 211 255
Line -1 false 144 100 70 87
Line -1 false 70 87 45 87
Line -1 false 45 86 26 97
Line -1 false 26 96 22 115
Line -1 false 22 115 25 130
Line -1 false 26 131 37 141
Line -1 false 37 141 55 144
Line -1 false 55 143 143 101
Line -1 false 141 100 227 138
Line -1 false 227 138 241 137
Line -1 false 241 137 249 129
Line -1 false 249 129 254 110
Line -1 false 253 108 248 97
Line -1 false 249 95 235 82
Line -1 false 235 82 144 100

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="bee1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>VR-per-flower-1</metric>
    <metric>VR-per-flower-2</metric>
    <metric>pollination-success-1</metric>
    <metric>pollination-success-2</metric>
    <metric>reward-1</metric>
    <metric>reward-2</metric>
    <metric>count-change-flightsteps</metric>
    <metric>count-change-reward</metric>
    <metric>changes-by-100-visits</metric>
    <enumeratedValueSet variable="frequency">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_bees">
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Flightsteps_until_change">
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flower_cover">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="renew_reward">
      <value value="1.0001"/>
      <value value="1.0005"/>
      <value value="1.001"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reward" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>visits1</metric>
    <metric>visits2</metric>
    <metric>reward-1</metric>
    <metric>reward-2</metric>
    <metric>number_flowers1</metric>
    <metric>number_flowers2</metric>
    <enumeratedValueSet variable="number_bees">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="renew_reward">
      <value value="1.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-angle">
      <value value="30"/>
      <value value="50"/>
      <value value="90"/>
      <value value="130"/>
      <value value="180"/>
      <value value="190"/>
      <value value="270"/>
      <value value="360"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
