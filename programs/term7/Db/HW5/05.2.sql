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
) AS S
