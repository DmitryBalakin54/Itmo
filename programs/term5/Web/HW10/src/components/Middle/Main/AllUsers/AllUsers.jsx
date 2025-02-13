import {useMemo} from "react";

const AllUsers = ({users}) => {

    const sortedUsers = useMemo(() => {
        return users.sort((a, b) => a.id - b.id)
    }, [users])

    return (
        <div className="users datatable">
            <div className="caption">User</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Login</th>
                    <th>Name</th>
                </tr>
                </thead>

                <tbody>
                {users.length !== 0
                    ?
                    sortedUsers.map((user) =>
                        <tr key={user.id}>
                            <td className="id">{user.id}</td>
                            <td className="login">{user.login}</td>
                            <td className="name">{user.name}</td>
                        </tr>
                    )
                    :
                    <tr className="noData">
                        <td colSpan="3">
                            No data
                        </td>
                    </tr>
                }
                </tbody>
            </table>
        </div>

    )
}


export default AllUsers;