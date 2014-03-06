(deftemplate my-world
	(multislot limit)
	(multislot ball)
	(multislot men)
	(slot id)
	(multislot moves)
	(slot direction)
)

(deftemplate bonus-world
	(multislot limit)
	(multislot ball)
	(multislot men)
	(slot id)
	(multislot moves)
	(slot direction)
)

	
(deffacts directii
	(direction 1 0 N)
	(direction -1 0 S)
	(direction 0 1 E)
	(direction 0 -1 W)
	(direction 1 1 NE)
	(direction -1 1 SE)
	(direction -1 -1 SW)
	(direction 1 -1 NW)
)	

(defrule init_world
		(declare (salience 100))
		?f <- (world (limit ?x ?y) 
						 (ball ?bx ?by) 
						 (men $?m) 
						 (id ?k) 
						 (moves $?mov)
				)
	=>
		(retract ?f)
		(assert (my-world (limit ?x ?y) 
								(ball ?bx ?by) 
								(men $?m) 
								(id ?k)
								(moves ?bx ?by - )
								(direction NIL)))
)
	
(defrule salt_jucator
		(direction ?dirx ?diry ?dir)
		(my-world (limit $?lim)
					 (ball ?bx ?by)
					 (men $?m1 ?mx&:(= (+ ?bx ?dirx) ?mx)
					 			  ?my&:(= (+ ?by ?diry) ?my)
					 		- $?m2)
					 (id ?k)
					 (moves $?mov)
					 (direction ?d&NIL|?dir))	
	=>
		(assert (my-world (limit $?lim)
								(ball (+ ?bx ?dirx) (+ ?by ?diry))
								(men $?m1 $?m2)
								(id ?k)
								(moves $?mov)
								(direction ?dir)))
)


(defrule salt_ground
		(direction ?dirx ?diry ?dir)
		(and
			?f <- (my-world (limit $?lim)
						 (ball ?bx ?by)
				 	 	 (men $?m)
						 (id ?k)
						 (moves $?mov)
						 (direction ?dir))
			(not (my-world (limit $?lim)
						 (ball ?bx ?by)
				 	 	 (men $?m1 ?mx&:(= (+ ?bx ?dirx) ?mx)
					 			     ?my&:(= (+ ?by ?diry) ?my)
					 		   - $?m2)
						 (id ?k)
						 (moves $?mov)
						 (direction ?dir)))
		)		
		=>
		(retract ?f)
		(assert (my-world (limit $?lim)
								(ball (+ ?dirx ?bx) (+ ?diry ?by))
								(men $?m)
								(id ?k)
								(moves $?mov (+ ?dirx ?bx) (+ ?diry ?by) - )
								(direction NIL)))		
)

(defrule winner
		(declare (salience 200))
		(my-world (limit ?x ?y)
					 (ball ?bx&:(= (- ?x 1) ?bx) ?by) 
					 (men $?) 
					 (id ?k) 
					 (moves $?m)
					 (direction ?d))
	=>
		(assert (win (id ?k) (moves $?m)))
)		
				 		
(defrule remove_directions
		(declare (salience -10))
		?f <- (direction $?x)		 		
	=>
		(retract ?f)
)			 		
				 		
(defrule remove_winning_worlds
		(win (id ?k) (moves $?mov))
		?f <- (my-world (limit ?x ?y)
					 	 	 (ball ?bx ?by) 
					 		 (men $?) 
					 		 (id ?k) 
					 		 (moves $?)
					 		 (direction ?d))	 		
	=>
		(retract ?f)
)				 		
				 		
(defrule remove_all_worlds
		(declare (salience -10))
		?f <- (my-world) 	
	=>
		(retract ?f)
)



