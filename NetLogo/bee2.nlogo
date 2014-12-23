; Masterthesis
; Helen Czioska



breed [bees bee]

bees-own [ 
  reward-memory   ;string - the bee has a memory for the reward to check for flower constancy
  array           ;Array - how far can the bee see on the grid? 
  choice          ;boolean - flower-constancy, which flower is preferred? Changes with distance (flight count) and bad reward
  flight-count    ;integer - flight-steps to find a flower
  flowers-visited ;integer - counter how many flowers this be already visited
  flower-memory   ;string - memory which floweres were already visited, memory is 10
  change-prob     ;float - propability to change the preferred flower type. Increases with low reward
  spec-last-visit ;boolean - remembers the species from lsat visit
  change-count1   ;integer - how often does this pollinator switch preferences?
  change-count2   ;integer - how often does this pollinator switch preferences?
  handling-time   ;integer - how long does the pollinator has to stay on the flower
  ]


patches-own [ 
  species       ;boolean - defined species, does not change
  reward        ;integer - reward to offer the pollinator, decreses with every visit and is slowly restored
  visit-count   ;integer - counter how often this flower was visited
  pollination-count ;integer - counter for every successfull pollination
  ]


globals[
  flower-number
  flower-number1
  flower-number2
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  
  ; there are 2 flower types on a meadow
  ; density and frequency can be adjusted with the sliders
  ; species are randomly distributed within the total flower_cover
  
   
;   ask patches [ifelse random-float 100 > flower-cover
;     [set pcolor green]
;     [ifelse random-float 100 < frequency 
;      [set species 1 set pcolor 47 set reward 1 ] 
;      [set species 2 set pcolor 17 set reward 1 ]
;     ] 
;     ]


; second approach with clusters:
 
 
 ask patches [set pcolor green set species 0]
 set flower-number round ((flower-cover / 100) * (count patches))
 set flower-number1 round (flower-number * (frequency / 100))
 set flower-number2 round (flower-number - flower-number1)
 
 ask n-of (flower-number1 / cluster-degree ) patches with [pcolor = green] [set species 1 set pcolor 47 set reward 1 ] 
 ask n-of (flower-number2 / cluster-degree ) patches with [pcolor = green] [set species 2 set pcolor 17 set reward 1 ]
 
 
 
 while [count patches with [species > 0] < flower-number ]
 [ask one-of patches with [species > 0] 
   [
    if (species = 1 and (count patches with [species = 1] < flower-number1))
      [if (any? neighbors with [species = 0])
        [ask one-of neighbors with [species = 0] [set species [species] of myself set pcolor [pcolor] of myself set reward 1 ]]
      ]
   
   if (species = 2 and (count patches with [species = 2] < flower-number2))
      [if (any? neighbors with [species = 0])
        [ask one-of neighbors with [species = 0] [set species [species] of myself set pcolor [pcolor] of myself set reward 1 ]]
      ]
   ]
 ]
 
 ;- die variable "Cluster-degree" von 1 bis number-flowers1 bzw number-flowers2 einsetzbar machen
 

  create-bees number-bees 
  [ ;set shape "bee"
    set shape "circle" set color 104 
    setxy random-xcor random-ycor 
    set reward-memory (list)
    set flight-count 0
    set flowers-visited 0
    set flower-memory (list)
    set handling-time 0
    
    let next-flower min-one-of patches with [species > 0 ][distance myself]
    face next-flower set choice [species] of next-flower ;sets the initial preference 
    ]

  reset-ticks
  
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  ; bee looks for specific flower (--> memory, constancy) in given view (cone-radius)
  ; bee moves towards nearest of the chosen flower type
  ; if there is no such type in the radius, bee moves randomly forward
  ; [random-normal 0 stdev-angle fd 1] for correlated random walk --> [random-nomral mean standarddeviation] (see Kareiva & Shinesada 1983, Bartumeus 2005, Codling 2008)
  ; Bees move with 0.1m/s (see Kunin 1991 in K&I 1996), one tick = 1sec & 1 grid-cell= 0.1m

to move

     ifelse (flight-count < flightsteps-until-change) ; after XY random movements, the bee justs goes to the next available flower
              [
                ; find out if there are any flowers in the visible area of the bee which have the preferres species AND were not visited before (= parallel visual scan Bukovac 2013)
                set array patches in-cone view 180 with [(species = [choice] of myself) and (not member? self [flower-memory] of myself) ] 
                let next-flower min-one-of array [distance myself]
                ifelse any? array 
                    [ face next-flower fd 1] ;face the nearest flower and move to it in a straight line (see Viswanathan 2008)
                    [ rt random-normal 0 stdev-angle fd 1 set flight-count flight-count + 1 ]
              ]
   
   
              [
                set array patches in-cone view 180 with [ (species > 0) and (not member? self [flower-memory] of myself) ] 
                let next-flower min-one-of array [distance myself]
                ifelse any? array
                    [ face next-flower fd 1 set choice [species] of next-flower set change-count2 change-count2 + 1]
                    [ rt random-normal 0 stdev-angle fd 1 set flight-count flight-count + 1 ]
              ]
  
end           
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to visit

  ; Option 1: 
  ; If the reward is in the lower quantile of the overall-reward the probability to change increases with every visit (see Chittka 1997)
  ; The propability to change the preferred species increases with 10% every time the flower has a low reward
  ; option 2: 
  ; The pollinator remembers the reward of the last 3 (4) visited flowers and compares this average with the current reward. (see Faruq2013)
  ; Is the reward worse than average the pollinator changes its preference
  ; The memory is working with a list
  ; Problem: If one species has a low reward, the average is also very low and the pollinator won´t change even if the other species has a much higher reward
  ; one solution: Occasionly tests oin other flowers (see Goulson1999) 
  
   if (length (reward-memory) > 0)
   [
      if (reward < (mean reward-memory) / 2) 
        [
          set change-prob change-prob + 0.2 
          ifelse (choice = 1)
            [ if (random-float 1 < change-prob) [set choice 2 set change-prob 0 set change-count1 change-count1 + 1]]
            [ if (random-float 1 < change-prob) [set choice 1 set change-prob 0 set change-count1 change-count1 + 1]]
        ]
        
      if (reward > (mean reward-memory) * 2) [set change-prob 0]
   ]
   
   ;remember reward
   while [(length reward-memory ) >= 4] [ set reward-memory  (but-first reward-memory ) ]
   set reward-memory  (lput [reward] of patch-here reward-memory  )
   
   ;remember flower-location
      ; Every pollinator has a memory for visited flowers. Pollinators avoid recently visited flowers (see Goulson1999 for review)
      ; Goulson 2000: "Pollinators can remember the location of the last 4 visited flowers"
   while [(length flower-memory) >= 4] [ set flower-memory (but-first flower-memory) ]
   set flower-memory (lput patch-here flower-memory )
   
   ;calculate handling-time
      ; 4 s/J (Roubik 1989 in Kunin & Iwasa 1996)
      ; 0.5 sec constant minimum handling time (see K&I 1996)
      ; handling time = reward * 4 + 0.5
   set handling-time (round (reward * 4 + 0.5))
   set flight-count 0                                ; every time a pollinator visits a flower, the flight-count is set to zero again
   set flowers-visited flowers-visited + 1

   ; flower-parameters
   if (spec-last-visit = [species] of patch-here) [set pollination-count pollination-count + 1] ; if the last visited flower was from the same species, pollination was successfull
   set reward 0                                      ; when a bee finds a flower, the reward is depleted
   set visit-count visit-count + 1                   
   
   set spec-last-visit ([species] of patch-here)    ; must update this parameter after checking for successful pollination
 

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to stay
  
set handling-time handling-time - 1

end  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to go
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report visits [spec]
report (sum [visit-count] of patches with [species = spec])
end

to-report pollination-success [spec]
report (sum [pollination-count] of patches with [species = spec]) / count patches with [species = spec]
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

to-report number_flowers [spec]
report count patches with [species = spec]
end
@#$#@#$#@
GRAPHICS-WINDOW
555
10
1211
687
50
50
6.4
1
10
1
1
1
0
1
0
1
-50
50
-50
50
0
0
1
ticks
30.0

SLIDER
17
106
189
139
number-bees
number-bees
0
100
37
1
1
NIL
HORIZONTAL

SLIDER
17
142
456
175
flower-cover
flower-cover
0
100
20
1
1
NIL
HORIZONTAL

BUTTON
18
69
81
102
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
126
70
189
103
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
17
178
189
211
frequency
frequency
0
100
44
1
1
NIL
HORIZONTAL

MONITOR
5
329
81
374
# Species 1
count patches with [species = 1]
17
1
11

MONITOR
6
375
81
420
# Species 2
count patches with [species = 2]
17
1
11

BUTTON
126
33
189
66
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

MONITOR
82
329
209
374
# bees with choice 1
count bees with [ choice = 1]
17
1
11

INPUTBOX
294
33
351
93
view
6
1
0
Number

SLIDER
17
216
189
249
stdev-angle
stdev-angle
0
90
65
1
1
NIL
HORIZONTAL

SLIDER
17
252
190
285
flightsteps-until-change
flightsteps-until-change
0
30
5
1
1
NIL
HORIZONTAL

PLOT
7
423
209
585
Visit-Count per Species
NIL
NIL
0.0
5.0
0.0
2.0
true
false
"" ""
PENS
"spec1" 1.0 0 -13840069 true "" "plot (sum [visit-count] of patches with [species = 1]) / (ticks + 1)"
"spec2" 1.0 0 -5825686 true "" "plot (sum [visit-count] of patches with [species = 2]) / (ticks + 1)"

MONITOR
82
375
209
420
# bees with choice 2
count bees with [choice = 2]
17
1
11

PLOT
216
423
416
585
VR per Flower
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"VR 1" 1.0 0 -8330359 true "" "plot (sum [visit-count] of patches with [species = 1]) / count patches with [species = 1]"
"VR 2" 1.0 0 -7858858 true "" "plot (sum [visit-count] of patches with [species = 2]) / count patches with [species = 2]"

PLOT
6
590
206
740
Mean Reward per Species
NIL
NIL
0.0
2.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -8330359 true "" "plot ( mean [reward] of patches with [species = 1])"
"pen-1" 1.0 0 -5825686 true "" "plot ( mean [reward] of patches with [species = 2])"

PLOT
215
588
415
738
count bees with [ choice = x]
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -8330359 true "" "plot count bees with [ choice = 1]"
"pen-1" 1.0 0 -5825686 true "" "plot count bees with [ choice = 2]"

PLOT
418
587
618
737
Pollination success
NIL
NIL
0.0
5.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -11085214 true "" "plot (sum [pollination-count] of patches with [species = 1]) / count patches with [species = 1]"
"pen-1" 1.0 0 -7858858 true "" "plot (sum [pollination-count] of patches with [species = 2]) / count patches with [species = 2]"

PLOT
420
424
620
585
Change-Reasons (green=flight-count, pink=reward)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot sum [change-count2] of bees"
"pen-1" 1.0 0 -7858858 true "" "plot sum [change-count1] of bees"

MONITOR
214
329
285
374
VR 1
(sum [visit-count] of patches with [species = 1]) / count patches with [species = 1]
17
1
11

MONITOR
214
376
286
421
VR 2
(sum [visit-count] of patches with [species = 2]) / count patches with [species = 2]
17
1
11

MONITOR
289
329
346
374
Poll 1
(sum [pollination-count] of patches with [species = 1]) / count patches with [species = 1]
17
1
11

MONITOR
290
376
347
421
Poll 2
(sum [pollination-count] of patches with [species = 2]) / count patches with [species = 2]
17
1
11

MONITOR
349
329
412
374
reward 1
( mean [reward] of patches with [species = 1])
17
1
11

MONITOR
349
376
412
421
reward 2
( mean [reward] of patches with [species = 2])
17
1
11

MONITOR
415
376
499
421
change reward
sum [change-count1] of bees
17
1
11

MONITOR
414
329
499
374
change flight
sum [change-count2] of bees
17
1
11

INPUTBOX
352
33
507
93
reward-function
4.0E-4
1
0
Number

INPUTBOX
237
33
293
93
number_species
2
1
0
Number

MONITOR
214
280
286
325
Sum Visits
sum [visit-count] of patches
17
1
11

PLOT
622
586
822
736
VR per tick
NIL
NIL
0.0
10.0
0.0
0.0010
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot ((sum [visit-count] of patches with [species = 1]) / count patches with [species = 1]) / (ticks + 1)"
"pen-1" 1.0 0 -7858858 true "" "plot ((sum [visit-count] of patches with [species = 2]) / count patches with [species = 2]) / (ticks + 1)"

SLIDER
340
208
512
241
cluster-degree
cluster-degree
1
200
9
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
