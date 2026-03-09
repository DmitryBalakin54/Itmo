SELECT StudentId, StudentName, GroupName
FROM Students NATURAL JOIN Groups NATURAL JOIN Clubs NATURAL JOIN ClubMembers
WHERE ClubName = :ClubName