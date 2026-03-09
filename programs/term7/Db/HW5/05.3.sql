SELECT StudentName, CourseName
FROM Courses
NATURAL JOIN (
    SELECT StudentId, StudentName, CourseId
    FROM Students
    NATURAL JOIN Plan

    EXCEPT

    SELECT StudentId, StudentName, CourseId
    FROM Students
    NATURAL JOIN Marks
    WHERE Mark = 4 or Mark = 5
) AS S
