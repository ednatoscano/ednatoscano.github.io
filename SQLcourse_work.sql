--_____________________________________________________________________________
-- QUERY 1-A

-- Retrieve the following information about the timetable classes of the group exercise classes
-- belonging to the category called Body maintenance: day of week, start time,
-- name of the group exercise class and duration.
-- Sort the result rows in ascending order by day of week, start time and name.
Mondays|08:00|Pilates 1|60
Mondays|08:10|Yoga 1|50
Mondays|13:00|Yoga 1|50
Mondays|20:10|Yoga 1|50
Tuesdays|08:10|Yoga 1|50


SELECT tt.day_of_week, tt.start_time, g.gc_name, g.duration
FROM tt_class as tt
  LEFT OUTER JOIN g_class as g
  ON g.gc_id = tt.gc_id
  LEFT OUTER JOIN category as c
  ON g.c_id = c.c_id
  WHERE c.c_name = "Body maintenance"
ORDER BY tt.day_of_week ASC, tt.start_time ASC, g.gc_name;

--_____________________________________________________________________________
-- QUERY 1-B
-- Retrieve the following information about the timetable classes of the group exercise classes
-- belonging to the Body Maintenance category,
-- with a difficulty level of 2 or a duration of at least 60 minutes: day of week, start time,
-- name of the group exercise class, duration and level.
-- Sort the result rows in ascending order by day of week, start time and name.
Mondays|08:00|Pilates 1|60|2
Mondays|08:10|Yoga 1|50|2
Mondays|13:00|Yoga 1|50|2
Mondays|20:10|Yoga 1|50|2
Tuesdays|08:10|Yoga 1|50|2

SELECT tt.day_of_week, tt.start_time, g.gc_name, g.duration, g.level
FROM tt_class as tt
  LEFT OUTER JOIN g_class as g
  ON g.gc_id = tt.gc_id
  LEFT OUTER JOIN category as c
  ON g.c_id = c.c_id
  WHERE c.c_name = "Body maintenance" AND (g.level = 2 OR g.duration >= 60)
ORDER BY tt.day_of_week ASC, tt.start_time ASC, g.gc_name;


--_____________________________________________________________________________
-- QUERY 2-A
-- Retrieve the following information for the shortest group exercise class: the name and duration of the class.
-- Sort the result rows in ascending order by the name of the class.
Strength|45


SELECT gc_name, duration
FROM g_class
WHERE duration = (SELECT MIN(duration)
                  FROM g_class)
ORDER BY gc_name ASC;


--_____________________________________________________________________________
-- QUERY 2-B
-- Retrieve the following information for the shortest group exercise class:
-- the name and duration of the class and the name of the category.
-- Sort the result rows in ascending order by the name of the class.
Strength|45|Muscular strength

SELECT g.gc_name, g.duration, c.c_name
FROM g_class as g
LEFT OUTER JOIN category as c
                ON g.c_id = c.c_id
WHERE g.duration = (SELECT MIN(duration)
                  FROM g_class)
ORDER BY gc_name ASC;


--_____________________________________________________________________________
-- QUERY 2-C
-- Retrieve the following information for the shortest group exercise class belonging to
-- the category called Body maintenance: the name and duration of the class and the name of the category.
-- Sort the result rows in ascending order by the name of the class.
Basics of yoga|50|Body maintenance
Yoga 1|50|Body maintenance

SELECT g.gc_name, g.duration, c.c_name
FROM g_class as g
LEFT OUTER JOIN category as c
                ON g.c_id = c.c_id
WHERE c_name IN
            (SELECT c_name
              FROM category
              WHERE category.c_name= "Body maintenance")
              AND duration IN
                          (SELECT MIN(duration)
                          FROM g_class
                          WHERE c.c_id = g_class.c_id)
ORDER BY gc_name ASC;



--_____________________________________________________________________________
-- QUERY 3
-- Retrieve ids and names of instructors who can instruct both classes in the category called Indoor cycling and
-- classes in the category called Body maintenance.
-- Sort the result rows in ascending order by the instructor id.
1|Elisa Markkanen


SELECT DISTINCT i.i_id, i.i_name
FROM instructor as i
LEFT OUTER JOIN can_instruct as can
                ON i.i_id = can.i_id
                LEFT OUTER JOIN g_class as g
                                ON can.gc_id = g.gc_id
                                LEFT OUTER JOIN category as c
                                                ON g.c_id = c.c_id
                                                WHERE c.c_name = "Indoor cycling"
INTERSECT
SELECT DISTINCT i.i_id, i.i_name
FROM instructor as i
LEFT OUTER JOIN can_instruct as can
                ON i.i_id = can.i_id
                LEFT OUTER JOIN g_class as g
                                ON can.gc_id = g.gc_id
                                LEFT OUTER JOIN category as c
                                                ON g.c_id = c.c_id
                                                WHERE c.c_name = "Body maintenance"

ORDER BY i.i_id ASC;


--_____________________________________________________________________________
-- QUERY 4-A
-- For each category, retrieve
-- the name of the category,
-- the number of group exercise classes belonging to the category,
-- as well as the minimum duration and
-- maximum duration of group exercise classes.
-- Sort the result rows in ascending order by the name of the category.
Body maintenance|3|50|60
Indoor cycling|2|60|80
Muscular strength|1|45|45

SELECT c.c_name, COUNT(g.c_id) as no_group_classes, MIN(g.duration) as min_duration, MAX(g.duration) as max_duration
FROM g_class as g
LEFT OUTER JOIN category as c
                 ON g.c_id = c.c_id
GROUP BY g.c_id
ORDER BY c.c_name ASC;



--_____________________________________________________________________________
-- QUERY 5
-- Retrieve IDs and names for instructors who instruct timetable classes of
-- all the group exercise classes they can instruct.
-- Sort the result rows in ascending order by the instructor ID.
3|Mikko Kontinen


SELECT i.i_id, i.i_name
FROM instructor as i, can_instruct as can
WHERE i.i_id = can.i_id
GROUP BY i.i_id
HAVING COUNT(*) = (SELECT COUNT(*)
                    FROM tt_class as tt
                    WHERE can.i_id = tt.i_id)
ORDER BY i.i_id ASC;
