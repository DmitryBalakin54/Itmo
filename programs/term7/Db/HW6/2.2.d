StudentsByClubName(StudentId, StudentName, GroupName) :-
    Students(StudentId, StudentName, GroupId),
    Groups(GroupId, GroupName),
    ClubMembers(ClubId, StudentId),
    Clubs(ClubId, ClubName, _),
    ClubName = :ClubName.