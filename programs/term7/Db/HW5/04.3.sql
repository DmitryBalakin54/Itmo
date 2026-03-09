SELECT StudentId, StudentName, GroupId
FROM Students
NATURAL JOIN Clubs
NATURAL JOIN ClubMembers
WHERE ClubName = :ClubName
EXCEPT 
    SELECT StudentId, StudentName, GroupId
    FROM Students
    NATURAL JOIN Marks
    NATURAL JOIN Courses
    WHERE CourseName = :CourseName