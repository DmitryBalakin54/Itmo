SELECT DISTINCT S.StudentId, S.StudentName, S.GroupId
FROM Students AS S, ClubMembers AS CM, Clubs AS C
WHERE S.StudentId = CM.StudentId AND CM.ClubId = C.ClubId AND C.ClubName = :ClubName 