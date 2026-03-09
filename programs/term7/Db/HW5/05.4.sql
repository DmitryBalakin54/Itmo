SELECT StudentName, CourseName
FROM Courses
NATURAL JOIN (
    SELECT DISTINCT StudentId, StudentName, CourseId
    FROM Students
    NATURAL JOIN Plan
    NATURAL JOIN Lecturers
    WHERE LecturerName = :LecturerName 
) AS S