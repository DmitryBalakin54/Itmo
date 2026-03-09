SELECT DISTINCT StudentId
FROM Marks
NATURAL JOIN Students

WHERE GroupId IN (
    SELECT GroupId
    FROM Lecturers
    NATURAL JOIN Plan
    WHERE LecturerName = :LecturerName
) 
AND StudentId NOT IN (
    SELECT StudentId
    FROM (
        SELECT StudentId, CourseId
        FROM (
            SELECT StudentId 
            FROM Marks
        ) AS C
        CROSS JOIN (
            SELECT CourseId
            FROM Plan
            NATURAL JOIN Lecturers
            WHERE LecturerName = :LecturerName 
        ) AS B

        EXCEPT

        SELECT StudentId, CourseId
        FROM Marks
    ) AS A
) 
