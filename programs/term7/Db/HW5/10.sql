SELECT T.StudentId, Total, coalesce(Passed, 0) AS Passed, Total - coalesce(Passed, 0) AS Failed
FROM (
    SELECT s.StudentId, coalesce(count(DISTINCT CourseId), 0) AS Total
    FROM Students AS s
    LEFT JOIN Plan AS pln ON pln.GroupId = s.GroupId 
    GROUP BY s.StudentId
) AS T
LEFT JOIN (
    SELECT ss.StudentId, coalesce(count(DISTINCT CourseId), 0) AS Passed
    FROM Marks
    NATURAL JOIN Plan
    NATURAL JOIN Students AS ss
    GROUP BY ss.StudentId
) AS P ON P.StudentId = T.StudentId
