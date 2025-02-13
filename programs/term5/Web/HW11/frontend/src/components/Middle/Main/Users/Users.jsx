import React, {useEffect, useMemo, useState} from 'react';
import axios from "axios";
import {dateFormat} from "../../../../utils.js"

const Users = () => {
    const [users, setUsers] = useState(null)

    useEffect(() => {
        axios.get("/api/users").then((response)=>{
            setUsers(response.data)
        }).catch((error)=>{
            console.log(error)
        })
    }, []);

    const sortedUsers = useMemo(() => {
        if (!users)
            return []
        return users.sort((a, b) => b.creationTime - a.creationTime)
    }, [users])



    return (
        <div className="users datatable">
            <div className="caption">User</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Login</th>
                    <th>Creation time</th>
                </tr>
                </thead>

                <tbody>
                {sortedUsers.length !== 0
                    ?
                    sortedUsers.map((user) =>
                        <tr key={user.id}>
                            <td className="id">{user.id}</td>
                            <td className="login">{user.login}</td>
                            <td className="creationTime">{dateFormat(user.creationTime)}</td>
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
};

export default Users;