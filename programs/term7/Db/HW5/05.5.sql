SELECT StudentName, CourseName
FROM Courses
NATURAL JOIN (
    SELECT DISTINCT StudentId, StudentName, CourseId
    FROM Students
    NATURAL JOIN Plan
    WHERE StudentId IN (
        SELECT StudentId
        FROM Clubs
        NATURAL JOIN ClubMembers
        WHERE ClubName = :ClubName
    )
) AS S