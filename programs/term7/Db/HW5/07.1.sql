SELECT CourseId, GroupId
FROM Marks
CROSS JOIN Students

EXCEPT

SELECT CourseId, GroupId
FROM (
    SELECT S.StudentId, CourseId, GroupId
    FROM Marks
    CROSS JOIN Students AS S

    EXCEPT

    SELECT S.StudentId, CourseId, GroupId
    FROM Marks
    NATURAL JOIN Students AS S
) AS M 