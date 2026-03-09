StudentsByCLubName(StudentId, StudentName, GroupId) :-
    Students(StudentId, StudentName, GroupId),
    ClubMembers(ClubId, StudentId),
    Clubs(ClubId, ClubName, _),
    ClubName = :ClubName.