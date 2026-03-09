SELECT DISTINCT S.StudentId, S.StudentName, G.GroupName
FROM Students AS S, Groups AS G, ClubMembers AS CM, Clubs AS C
WHERE S.GroupId = G.GroupId AND CM.StudentId = S.StudentId AND C.ClubId = CM.ClubId AND C.ClubName = :ClubName