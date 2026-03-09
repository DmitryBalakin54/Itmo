SELECT DISTINCT StudentId
FROM Marks

EXCEPT

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