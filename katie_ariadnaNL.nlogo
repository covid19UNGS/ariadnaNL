breed [personas1 persona1]        ; living personas 0-17
breed [personas2 persona2]        ; living personas 18-34
breed [personas3 persona3]        ; living personas 35-64
breed [personas4 persona4]        ; living personas 65+

turtles-own [ estado            ; 1 suceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado
                                ; 9 infectado-leve
               donde            ; 1=casa, 2=trabajo, 3=escuela, 4=hospital

               mi-casa
               mi-trabajo
               mi-escuela
             ]
patches-own [ lugar             ;; 1=casa, 2=trabajo, 3=escuela, 4=hospital
              nro-personas
              nro-personas1
              nro-personas2
              nro-personas3
              nro-personas4
              mu_ie             ;; de infectados a latentes del parche (counts the # transitioning from exposed to)
              fallecidos        ;; Fallecidos de esa casa
]

globals [ total-patches
          cant-trabajos
          cant-casas
          cant-escuela
          hospital             ; patch hospital
          horas-en-casa        ; horas en el trabajo
          horas-de-dormir
          gamma                ; periodo-latencia
          lambda_p             ; periodo-presintomatico
          lambda_a             ; periodo-asintomatico
          lambda_h             ; periodo-pre-hospitalizacion ( asintomatico - presintomatico)
          rho_d                ; hospitalizacion de los que fallecen
          rho_r                ; hospitalizacion de los recuperados
          nro-fallecidos       ;
          nro-recuperados      ;
          nro-hospitalizados
          nro-casos-sintomaticos

          tasa-de-ataque       ; por casa calculada al final de la epidemia
]

extensions [profiler table]

to setup-ini
  clear-all
  set total-patches count patches
  set-default-shape personas1 "triangle"
  set-default-shape personas2 "square"
  set-default-shape personas3 "circle"
  set-default-shape personas4 "wheel"
  ;;
  ;; Generar casas trabajos como parches
  ;;
  ;; 2/3 de casas 1/3 de trabajos 1 hospital 3 escuela
  ;;
  set cant-trabajos total-patches / 3
  set cant-escuela 3
  set cant-casas total-patches - cant-trabajos - cant-escuela - 1 + 1;; 
  ;; show (word cant-trabajos " " cant-casas)

  ask n-of cant-casas patches [
    set lugar 1
    set pcolor magenta
  ]
  ask n-of cant-trabajos patches with [lugar = 0] [
    set lugar 2
    set pcolor blue
  ]
  ask n-of cant-escuela patches with [lugar = 0] [
    set lugar 3
    set pcolor orange
  ]
  ask patches with [lugar = 0] [      ;; more than 1 hospital is being created - not sure why
    set lugar 4
    set pcolor lime
    set hospital self
  ]

  ;;
  ;; Sets age distribution (1) 0-17 anos, (2) 18-34, (3) 35-64, (5) 65+
  ;;
  ;set prop-personas1 0.19550259   ;; CABA
  ;set prop-personas2 0.276438152  ;; CABA
  ;set prop-personas3 0.364029423  ;; CABA
  ;set prop-personas4 0.164029838  ;; CABA

  ;set prop-personas1 0.298076286  ;; Buenos Aires
  ;set prop-personas2 0.269693078  ;; Buenos Aires
  ;set prop-personas3 0.325270827  ;; Buenos Aires
  ;set prop-personas4 0.106959809  ;; Buenos Aires

  ;set prop-personas1 0.205723239  ;; NYC
  ;set prop-personas2 0.261033318  ;; NYC
  ;set prop-personas3 0.379564407  ;; NYC
  ;set prop-personas4 0.153679036  ;; NYC

  ;set prop-personas1 0.10         ;; Japan-like
  ;set prop-personas2 0.25         ;; Japan-like
  ;set prop-personas3 0.35         ;; Japan-like
  ;set prop-personas4 0.30         ;; Japan-like

  ;set prop-personas1 0.40         ;; India-like
  ;set prop-personas2 0.25         ;; India-like
  ;set prop-personas3 0.25         ;; India-like
  ;set prop-personas4 0.10         ;; India-like

  ;;
  ;; Poner las personas en las casas
  ;;
  ask patches with [lugar = 1] [
      let personas1-por-casa ((1 + random max-personas-por-casa) * prop-personas1) ;; 0-17 anos
      sprout-personas1 personas1-por-casa [
        set size .5
        set donde 1
        set estado 1 ;; suceptible
        set mi-casa patch-here
        ;set-color-persona
        fd 0.2
      ]
      let personas2-por-casa ((1 + random max-personas-por-casa) * prop-personas2) ;; 18-34 anos
      sprout-personas2 personas2-por-casa [
        set size .5
        set donde 1
        set estado 1 ;; suceptible
        set mi-casa patch-here
        ;set-color-persona
        fd 0.2
      ]
      let personas3-por-casa ((1 + random max-personas-por-casa) * prop-personas3) ;; 35-64 anos
      sprout-personas3 personas3-por-casa [
        set size .5
        set donde 1
        set estado 1 ;; suceptible
        set mi-casa patch-here
        ;set-color-persona
        fd 0.2
      ]
      let personas4-por-casa ((1 + random max-personas-por-casa) * prop-personas4) ;; 65+ anos
      sprout-personas4 personas4-por-casa [
        set size .5
        set donde 1
        set estado 1 ;; suceptible
        set mi-casa patch-here
        ;set-color-persona
        fd 0.2
      ]
  ]
  set gamma    1 - exp ( - 1 / periodo-latencia )
  set lambda_p 1 - exp ( - 1 / periodo-presintomatico )
  set lambda_a 1 - exp ( - 1 / periodo-asintomatico )
  ;
  set lambda_h 1 - exp ( - 1 /  periodo-pre-hospitalizacion )    ;; Periodo entre que sos sintomatico y te hospitalizan

  set rho_d    1 - exp ( - 1 / periodo-hospitalizacion-fallecido )
  set rho_r    1 - exp ( - 1 / periodo-hospitalizacion-recuperado )

  set horas-de-dormir 8
  set horas-en-casa 24 - horas-de-dormir - horas-en-trabajo - horas-en-viaje    ;; Assume children go to school for the same #hrs as adults go to work

end

;; Sets a random population of patches
;;
;;
to setup
  setup-ini

  ;; inicializa con Latentes
  ;;
  ask n-of infectados-iniciales turtles
  [
    set estado 2
    ;set-color-persona
  ]

  reset-ticks

end

to go

  ;; Calcula proporcion de horas en casa o trabajo
  ;;
  let prop-horas-en-trabajo horas-en-trabajo / ( 24 - horas-de-dormir )
  let prop-horas-en-casa horas-en-casa / ( 24 - horas-de-dormir )
  let prop-horas-en-viaje horas-en-viaje / ( 24 - horas-de-dormir )

  ;;print (word "Prop trabajo: " prop-horas-en-trabajo)
  ;;print (word "Prop casa: " prop-horas-en-casa)
  ;;print (word "Prop viaje: " prop-horas-en-viaje)

  if ticks = 730 ;; simulation runs for 365 days
  [
    set tasa-de-ataque mean [ ( count turtles-here with [ estado > 1 ] + fallecidos ) / nro-personas ] of patches with [lugar = 1 and nro-personas > 0]
    stop
  ]
  ;;
  ;; Primero el movimiento de casa a trabajo al principio del dia
  ;;
  ask personas2 [
    ir-al-trabajo
  ]
  ask personas3 [
    ir-al-trabajo
  ]
  ;;
  ;; La infección en el viaje toma la proporcion de infectados 3 y 4 globales
  ;;
  infeccion-viaje prop-horas-en-viaje

  ;;
  ;; Infeccion en el trabajo
  ;;
  infeccion-local prop-horas-en-trabajo

  ask personas1 [
  ir-a-la-escuela
  ]

  ask turtles [
    volver-a-casa
 ]
 ;;
 ;; Infeccion en casa
 ;;
 infeccion-local-estado prop-horas-en-casa

  tick
end

to ir-al-trabajo
    if estado < 5 or estado = 8
    [  ; 1 suceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado, 9 infectado leve
    ;;
    ;; La primera vez asigna el lugar de trabajo
    ;;
    ifelse mi-trabajo = 0 [
      set mi-trabajo one-of patches with  [lugar = 2 and nro-personas < max-personas-por-trabajo]
      set donde 2
      move-to mi-trabajo
      set nro-personas nro-personas + 1
      ;;show  word mi-trabajo nro-personas    
    ]
    [
      (ifelse donde = 1 [
        move-to mi-trabajo
        ;;lt 10
        ;;fd 0.2
        set donde 2
        ]
        donde = 2 [
          show (word "Nadie se queda a dormir en el trabajo! Estado " estado)
        ]
       )
    ]
  ]
end

to ir-a-la-escuela
    if estado < 5 or estado = 8
    [  ; 1 susceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado, 9 infectado leve
    ;;
    ;; La primera vez asigna el lugar de escuela
    ;;
    ifelse mi-escuela = 0 [
      set mi-escuela one-of patches with  [lugar = 3] ;; for now, we do not set a max-personas-por-escuela
      set donde 3
      move-to mi-escuela ;; this command is not working as expected
      set nro-personas nro-personas + 1
    ]
    [
      (ifelse donde = 1 [
        move-to mi-escuela
        ;;lt 10
        ;;fd 0.2
        set donde 3
        ]
        donde = 3 [
          show (word "Nadie se queda a dormir en la escuela! Estado " estado)
        ]
       )
    ]
  ]
end

to volver-a-casa
   (ifelse estado < 6 or estado = 8
    [  ;1 suceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado, 9 infectado leve
      if donde != 1 [
        move-to mi-casa
        ;;rt 10
        ;;fd 0.2
        set donde 1
        set nro-personas nro-personas + 1
      ]
   ]
   estado = 6 [
     if donde != 4
     [
        move-to hospital
        ;;fd 0.2
        set donde 4
        set nro-personas nro-personas + 1
     ]
   ]
   estado = 7 [
     set nro-personas nro-personas - 1
     set nro-fallecidos nro-fallecidos + 1
     move-to mi-casa
     set fallecidos fallecidos + 1
     ;show (word "Fallecidos : " fallecidos "en " mi-casa)
     ;;ask mi-casa [ set fallecidos fallecidos + 1 ]
     die
   ]
   estado = 9 [
     if donde != 1
     [
        ;;print "Infectado leve se va a la casa"
        move-to mi-casa
        ;;fd 0.2
        set donde 1
        set nro-personas nro-personas + 1
     ]
  ]
  )

end


;;1 suceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado
;;
to infeccion-local [prop-horas]   ;; initially, all are susceptible (estado = 1) so you need to transition some to exposed (estado = 2)

  ask patches [
    set nro-personas count turtles-here
    set nro-personas1 count personas1-here
    set nro-personas2 count personas2-here
    set nro-personas3 count personas3-here
    set nro-personas4 count personas4-here
    if nro-personas > 0
    [
      let nro-infectores count turtles-here with [estado > 2 and estado < 6]
      set mu_ie  1 - exp( - beta * nro-infectores / nro-personas * prop-horas )
      ;;show (word "N: " nro-personas " I: " nro-infectores " mu_ie: " mu_ie )
    ]
  ]
  ask turtles [
    if estado = 1 [ ;; Susceptible

      if random-float 1 < mu_ie [     ;; if random reported number (b/w 0-1) is less than the Pr of contacting another and getting infected (mu_ie), then...

        set estado 2
        ;;show (word "Clocal 1 N: " nro-personas " mu_ie: " mu_ie )

      ]
    ]
  ]
end

;;1 suceptible, 2 latente, 3 presintomatico , 4 asintomatico, 5 sintomatico, 6 hospitalizado,  7 fallecido, 8 recuperado, 9 infectado-leve
;;
to infeccion-local-estado [prop-horas]

  ask patches [ 
    set nro-personas count turtles-here 
    if nro-personas > 0 ;; only applies to mi-casa b/c this command comes after volver-a-casa
    [
      let nro-infectores count turtles-here with [estado > 2 and estado < 6]
      set mu_ie  1 - exp( - beta * nro-infectores / nro-personas * prop-horas )
      ;show (word "N: " nro-personas " I: " nro-infectores " mu_ie: " mu_ie )
    ]
  ]
  ask turtles [
    (ifelse estado = 1 [ ;; Suceptible

      if random-float 1 < mu_ie [

        set estado 2
        ;;show (word "Clocal 2 N: " nro-personas " mu_ie: " mu_ie )


      ]
     ]
     estado = 2 [
        if random-float 1 < gamma
        [
          ifelse random-float 1 < proporcion-asintomaticos
          [
            set estado 4
          ][
            set estado 3
          ]
        ]
     ]
     estado = 3 [
        if random-float 1 < lambda_p
        [
          ;show "Entra en estado 5 Sintomatico"
          set estado 5
          set nro-casos-sintomaticos nro-casos-sintomaticos + 1
        ]
     ]
     estado = 4 [
        if random-float 1 < lambda_a
        [
          set estado 8
          set nro-recuperados nro-recuperados + 1
        ]
     ]
     estado = 5 [                                    ;; Si se supera la cantidad de camas pasaria a 7 u 8 proporcion-fallecimiento-saturado
        if random-float 1 < lambda_h
        [
          ifelse random-float 1 < proporcion-hospitalizados
          [
            ;show "Entra en estado 6 Hospitalizado"
            set estado 6
            set nro-hospitalizados nro-hospitalizados + 1
          ][
            ifelse random-float 1 < fallecido-sin-hospitalizacion [
              set estado 7
              ;show "Fallecido sin hospitalizar"
            ]
            [
              set estado 9                            ;; infectado leve se vuelve a casa a hacer cuarentena NO CONTAGIA
              ;show "Entra en estado 9 Leve"
            ]
          ]
        ]
     ]
     estado = 6 [
        if-else nro-hospitalizados > capacidad-de-camas [
          ;print "Estamos saturados!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          if-else random-float 1 < proporcion-fallecimiento-saturada [
            if random-float 1 < rho_d [
              set estado 7
              set nro-hospitalizados nro-hospitalizados - 1
            ]
          ][
            if random-float 1 < rho_r [
              set estado 8
              set nro-recuperados nro-recuperados + 1
              set nro-hospitalizados nro-hospitalizados - 1
            ]
          ]

        ][
          if-else random-float 1 < proporcion-fallecimiento-hospitalizados [
            if random-float 1 < rho_d [
              set estado 7
              set nro-hospitalizados nro-hospitalizados - 1
              ;;show "Sale de estado 6 a Fallecido"
            ]
          ][
            if random-float 1 < rho_r [
              set estado 8
              set nro-recuperados nro-recuperados + 1
              set nro-hospitalizados nro-hospitalizados - 1
              ;;show "Sale de estado 6 a Recuperado"
            ]
          ]
        ]
     ]
     estado = 9 [                 ;; infectado-leve
       if random-float 1 < rho_r [
          ;;show (word "Entró en estado 8 donde " donde)
          set estado 8
          set nro-recuperados nro-recuperados + 1
       ]
     ]
    )
    ;set-color-persona

  ]

end

to infeccion-viaje [prop-horas]   ;; includes only personas1-3 AND estados 3 or 4 b/c once they're symptomatic (5), they're bound to be home or in the hospital (and not commuting)
  let viajeros (turtle-set personas1 personas2 personas3)
  let nro-total-infectores count viajeros with [estado = 3 or estado = 4]
  let nro-total-personas count viajeros
  let mu_vi  1 - exp( - beta * nro-total-infectores / nro-total-personas * prop-horas)
  ;print (word "N: " nro-total-personas "I " nro-total-infectores "\n mu_vi: " mu_vi)
  ask viajeros [
    if estado = 1 [ ;; Susceptible

      if random-float 1 < mu_vi [

        set estado 2
        ;;show (word "Cviaje N: " nro-total-infectores  " mu_vi: " mu_vi )

      ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
355
10
767
423
-1
-1
4.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

SLIDER
10
55
232
88
max-personas-por-casa
max-personas-por-casa
4
10
10.0
1
1
NIL
HORIZONTAL

BUTTON
240
150
313
183
NIL
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

SLIDER
10
210
185
243
beta
beta
0
1
0.37
.01
1
NIL
HORIZONTAL

BUTTON
240
190
303
223
NIL
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
10
13
232
46
infectados-iniciales
infectados-iniciales
1
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
10
250
202
283
periodo-latencia
periodo-latencia
1
6
3.6
.1
1
NIL
HORIZONTAL

SLIDER
10
93
252
126
max-personas-por-trabajo
max-personas-por-trabajo
1
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
10
130
182
163
Horas-en-trabajo
Horas-en-trabajo
1
10
7.5
.1
1
NIL
HORIZONTAL

MONITOR
965
10
1115
55
Total de personas (0-17)
count personas1
1
1
11

MONITOR
1230
65
1337
110
Total de casas
cant-casas
0
1
11

BUTTON
240
230
342
263
go profiler
setup\nprofiler:start\nrepeat 365 [go]\nprofiler:stop          ;; stop profiling\nprint profiler:report  ;; view the results\nprofiler:reset         ;; clear the 
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
815
65
1215
340
Casos
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Latentes" 1.0 0 -8990512 true "" "plot count turtles with [estado = 2]"
"Presintomatico" 1.0 0 -955883 true "" "plot count turtles with [estado = 3]"
"Asintomatico" 1.0 0 -1184463 true "" "plot count turtles with [estado = 4]"
"Sintomatico" 1.0 0 -2674135 true "" "plot count turtles with [estado = 5]"
"Hospitalizado" 1.0 0 -10899396 true "" "plot nro-hospitalizados"
"Fallecido" 1.0 0 -16449023 true "" "plot nro-fallecidos"
"Leve" 1.0 0 -5509967 true "" "plot count turtles with [estado = 9]"

SLIDER
10
290
247
323
Proporcion-asintomaticos
Proporcion-asintomaticos
0
1
0.43
.01
1
NIL
HORIZONTAL

SLIDER
10
330
232
363
periodo-presintomatico
periodo-presintomatico
1
5
1.5
.1
1
NIL
HORIZONTAL

SLIDER
10
370
217
403
periodo-asintomatico
periodo-asintomatico
1
10
6.9
.1
1
NIL
HORIZONTAL

SLIDER
10
450
297
483
periodo-hospitalizacion-fallecido
periodo-hospitalizacion-fallecido
1
15
13.2
.1
1
NIL
HORIZONTAL

SLIDER
10
530
347
563
Proporcion-fallecimiento-hospitalizados
Proporcion-fallecimiento-hospitalizados
0
1
0.12
.01
1
NIL
HORIZONTAL

SLIDER
10
490
317
523
periodo-hospitalizacion-recuperado
periodo-hospitalizacion-recuperado
1
15
15.0
.1
1
NIL
HORIZONTAL

MONITOR
1230
245
1342
290
Total Fallecidos
nro-fallecidos
2
1
11

SLIDER
10
170
182
203
Horas-en-viaje
Horas-en-viaje
0
4
3.0
.1
1
NIL
HORIZONTAL

MONITOR
975
355
1125
400
Latentes (0-17)
count personas1 with [estado = 2]
2
1
11

MONITOR
975
415
1127
460
Presintomaticos (0-17)
count personas1 with [estado = 3]
2
1
11

MONITOR
975
475
1125
520
Asintomaticos (0-17)
count personas1 with [estado = 4]
2
1
11

MONITOR
975
535
1125
580
Sintomaticos (0-17)
count personas1 with [estado = 5]
2
1
11

MONITOR
1230
295
1340
340
Recuperados
nro-recuperados
2
1
11

MONITOR
1410
145
1482
190
Letalidad
nro-fallecidos / ( nro-recuperados + nro-fallecidos ) * 100
3
1
11

SLIDER
10
410
232
443
periodo-pre-hospitalizacion
periodo-pre-hospitalizacion
1
15
2.5
.1
1
NIL
HORIZONTAL

SLIDER
10
610
180
643
capacidad-de-camas
capacidad-de-camas
0
3000
225.0
1
1
NIL
HORIZONTAL

SLIDER
10
650
307
683
proporcion-fallecimiento-saturada
proporcion-fallecimiento-saturada
0
1
0.4
0.01
1
NIL
HORIZONTAL

MONITOR
1230
195
1362
240
NIL
nro-hospitalizados
2
1
11

SLIDER
10
570
252
603
proporcion-hospitalizados
proporcion-hospitalizados
0
1
0.1
.01
1
NIL
HORIZONTAL

SLIDER
195
610
410
643
fallecido-sin-hospitalizacion
fallecido-sin-hospitalizacion
0
1
0.006
0.001
1
NIL
HORIZONTAL

MONITOR
975
595
1125
640
Infectado leve (0-17)
count personas1 with [estado = 9]
2
1
11

MONITOR
1230
145
1395
190
NIL
nro-casos-sintomaticos
2
1
11

MONITOR
1125
10
1280
55
Total de personas (18-34)
count personas2
1
1
11

MONITOR
1290
10
1445
55
Total de personas (35-64)
count personas3
17
1
11

MONITOR
1455
10
1600
55
Total de personas (65+)
count personas4
17
1
11

MONITOR
1135
355
1285
400
Latentes (18-34)
count personas2 with [estado = 2]
17
1
11

MONITOR
1295
355
1440
400
Latentes (35-64)
count personas3 with [estado = 2]
17
1
11

MONITOR
1450
355
1595
400
Latentes (65+)
count personas4 with [estado = 2]
17
1
11

MONITOR
1135
415
1285
460
Presintomaticos (18-34)
count personas2 with [estado = 3]
17
1
11

MONITOR
1295
415
1440
460
Presintomatics (35-64)
count personas3 with [estado = 3]
17
1
11

MONITOR
1450
415
1595
460
Presintomaticos (65+)
count personas4 with [estado = 3]
17
1
11

MONITOR
1135
475
1285
520
Asintomaticos (18-34)
count personas2 with [estado = 4]
17
1
11

MONITOR
1295
475
1440
520
Asintomaticos (35-64)
count personas3 with [estado = 4]
17
1
11

MONITOR
1450
475
1595
520
Asintomaticos (65+)
count personas4 with [estado = 4]
17
1
11

MONITOR
1135
535
1285
580
Sintomaticos (18-34)
count personas2 with [estado = 5]
17
1
11

MONITOR
1295
535
1440
580
Sintomaticos (35-64)
count personas3 with [estado = 5]
17
1
11

MONITOR
1450
535
1595
580
Sintomaticos (65+)
count personas4 with [estado = 5]
17
1
11

MONITOR
1135
595
1282
640
Infectado leve (18-34)
count personas2 with [estado = 9]
17
1
11

MONITOR
1295
595
1442
640
Infectado leve (35-64)
count personas3 with [estado = 9]
17
1
11

MONITOR
1450
595
1595
640
Infectado leve (65+)
count personas4 with [estado = 9]
17
1
11

MONITOR
815
10
955
55
Total de personas
count turtles
17
1
11

MONITOR
815
355
965
400
Latentes
count turtles with [estado = 2]
17
1
11

MONITOR
815
415
965
460
Presintomaticos
count turtles with [estado = 3]
17
1
11

MONITOR
815
475
965
520
Asintomaticos
count turtles with [estado = 4]
17
1
11

MONITOR
815
535
965
580
Sintomaticos
count turtles with [estado = 5]
17
1
11

MONITOR
815
595
965
640
Infectado leve
count turtles with [estado = 9]
17
1
11

MONITOR
1350
65
1462
110
Total de escuela
cant-escuela
17
1
11

MONITOR
1475
65
1592
110
Total de trabajos
cant-trabajos
0
1
11

SLIDER
545
440
765
473
prop-personas1
prop-personas1
0
1
0.205723239
0.00000001
1
NIL
HORIZONTAL

SLIDER
545
480
765
513
prop-personas2
prop-personas2
0
1
0.261033318
0.00000001
1
NIL
HORIZONTAL

SLIDER
545
520
765
553
prop-personas3
prop-personas3
0
1
0.379564407
0.00000001
1
NIL
HORIZONTAL

SLIDER
545
560
765
593
prop-personas4
prop-personas4
0
1
0.153679036
0.00000001
1
NIL
HORIZONTAL

@#$#@#$#@
## Modelo de COVID - 19

Es un modelo que asume una red Bipartita de casas y trabajos, las personas van de sus casas al trabajo y se pueden contagiar en ambos lados, ademas tambien se pueden contagiar en el viaje.

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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Simulation CABA" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>count turtles with [estado = 1]</metric>
    <metric>count turtles with [estado = 2]</metric>
    <metric>count turtles with [estado = 3]</metric>
    <metric>count turtles with [estado = 4]</metric>
    <metric>count turtles with [estado = 5]</metric>
    <metric>count turtles with [estado = 6]</metric>
    <metric>count turtles with [estado = 7]</metric>
    <metric>count turtles with [estado = 8]</metric>
    <metric>count turtles with [estado = 9]</metric>
    <metric>nro-casos-sintomaticos</metric>
    <metric>nro-hospitalizados</metric>
    <metric>nro-fallecidos</metric>
    <metric>nro-recuperados</metric>
    <enumeratedValueSet variable="capacidad-de-camas">
      <value value="225"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-viaje">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-latencia">
      <value value="3.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-trabajo">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-fallecimiento-saturada">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.37"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-pre-hospitalizacion">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-presintomatico">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-casa">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-fallecimiento-hospitalizados">
      <value value="0.12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-recuperado">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-asintomaticos">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-hospitalizados">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-trabajo">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fallecido-sin-hospitalizacion">
      <value value="0.006"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-fallecido">
      <value value="13.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectados-iniciales">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-asintomatico">
      <value value="6.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas1">
      <value value="0.19550259"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas2">
      <value value="0.276438152"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas3">
      <value value="0.364029423"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas4">
      <value value="0.164029838"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Simulation NYC" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>count turtles with [estado = 1]</metric>
    <metric>count turtles with [estado = 2]</metric>
    <metric>count turtles with [estado = 3]</metric>
    <metric>count turtles with [estado = 4]</metric>
    <metric>count turtles with [estado = 5]</metric>
    <metric>count turtles with [estado = 6]</metric>
    <metric>count turtles with [estado = 7]</metric>
    <metric>count turtles with [estado = 8]</metric>
    <metric>count turtles with [estado = 9]</metric>
    <metric>nro-casos-sintomaticos</metric>
    <metric>nro-hospitalizados</metric>
    <metric>nro-fallecidos</metric>
    <metric>nro-recuperados</metric>
    <enumeratedValueSet variable="capacidad-de-camas">
      <value value="225"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-viaje">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-latencia">
      <value value="3.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-trabajo">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-fallecimiento-saturada">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.37"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-pre-hospitalizacion">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-presintomatico">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-casa">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-fallecimiento-hospitalizados">
      <value value="0.12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-recuperado">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-asintomaticos">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-hospitalizados">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-trabajo">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fallecido-sin-hospitalizacion">
      <value value="0.006"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-fallecido">
      <value value="13.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectados-iniciales">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-asintomatico">
      <value value="6.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas1">
      <value value="0.205723239"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas2">
      <value value="0.261033318"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas3">
      <value value="0.379564407"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas4">
      <value value="0.153679036"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Simulation Japan" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>count turtles with [estado = 1]</metric>
    <metric>count turtles with [estado = 2]</metric>
    <metric>count turtles with [estado = 3]</metric>
    <metric>count turtles with [estado = 4]</metric>
    <metric>count turtles with [estado = 5]</metric>
    <metric>count turtles with [estado = 6]</metric>
    <metric>count turtles with [estado = 7]</metric>
    <metric>count turtles with [estado = 8]</metric>
    <metric>count turtles with [estado = 9]</metric>
    <metric>nro-casos-sintomaticos</metric>
    <metric>nro-hospitalizados</metric>
    <metric>nro-fallecidos</metric>
    <metric>nro-recuperados</metric>
    <enumeratedValueSet variable="capacidad-de-camas">
      <value value="225"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-viaje">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-latencia">
      <value value="3.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-trabajo">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-fallecimiento-saturada">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.37"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-pre-hospitalizacion">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-presintomatico">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-casa">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-fallecimiento-hospitalizados">
      <value value="0.12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-recuperado">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-asintomaticos">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-hospitalizados">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-trabajo">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fallecido-sin-hospitalizacion">
      <value value="0.006"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-fallecido">
      <value value="13.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectados-iniciales">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-asintomatico">
      <value value="6.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas1">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas2">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas3">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas4">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Simulation India" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>ticks</metric>
    <metric>count turtles with [estado = 1]</metric>
    <metric>count turtles with [estado = 2]</metric>
    <metric>count turtles with [estado = 3]</metric>
    <metric>count turtles with [estado = 4]</metric>
    <metric>count turtles with [estado = 5]</metric>
    <metric>count turtles with [estado = 6]</metric>
    <metric>count turtles with [estado = 7]</metric>
    <metric>count turtles with [estado = 8]</metric>
    <metric>count turtles with [estado = 9]</metric>
    <metric>nro-casos-sintomaticos</metric>
    <metric>nro-hospitalizados</metric>
    <metric>nro-fallecidos</metric>
    <metric>nro-recuperados</metric>
    <enumeratedValueSet variable="capacidad-de-camas">
      <value value="225"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-viaje">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-latencia">
      <value value="3.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Horas-en-trabajo">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-fallecimiento-saturada">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.37"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-pre-hospitalizacion">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-presintomatico">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-casa">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-fallecimiento-hospitalizados">
      <value value="0.12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-recuperado">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Proporcion-asintomaticos">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporcion-hospitalizados">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-personas-por-trabajo">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fallecido-sin-hospitalizacion">
      <value value="0.006"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-hospitalizacion-fallecido">
      <value value="13.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infectados-iniciales">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodo-asintomatico">
      <value value="6.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas1">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas2">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas3">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-personas4">
      <value value="0.1"/>
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
1
@#$#@#$#@
