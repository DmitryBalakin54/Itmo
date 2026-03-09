SELECT DISTINCT StudentId
FROM Students
NATURAL JOIN Marks
NATURAL JOIN Plan
NATURAL JOIN Lecturers
WHERE LecturerName = :LecturerName