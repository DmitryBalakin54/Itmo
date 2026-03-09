ClubIdByHeadName(ClubId) :-
    Students(StudentId, StudentName, _),
    Clubs(ClubId, _, ClubStudentHeadId),
    ClubStudentHeadId = StudentId,
    StudentName = :StudentName.

StudentsByClubHeadName(StudentId, StudentName, GroupName) :-
    Students(StudentId, StudentName, GroupId),
    Groups(GroupId, GroupName),
    ClubMembers(ClubId, StudentId),
    ClubIdByHeadName(ClubId).