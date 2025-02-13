<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="isAdmin" type="Boolean" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">Users</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <#if isAdmin>
                    <th>Admin</th>
                </#if>
            </tr>
            </thead>
            <tbody>
            <template>
                <tr>
                    <td class="user-id"></td>
                    <td class="user-login"></td>
                    <#if isAdmin>
                        <td class="user-admin-status">
                            <button class="toggle-admin"></button>
                        </td>
                    </#if>
                </tr>
            </template>
            <tr class="noData">
                <td colspan="3">
                    No data
                </td>
            </tr>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            const $noData = $(".users .noData");
            $noData.find("td").text("Loading...");

            ajax({ action: "findAll" }, function(response) {
                response["users"].forEach(function(user) {
                    $noData.hide();
                    const $tr = $($(".users template").prop("content")).clone();
                    $tr.find(".user-id").text(user["id"]);
                    $tr.find(".user-login").text(user["login"]);

                    <#if isAdmin>
                    const isAdmin = user["admin"];
                    $tr.find(".toggle-admin")
                        .text(isAdmin ? "Disable" : "Enable")
                        .data("admin", isAdmin)
                        .data("user-id", user["id"]);
                    </#if>

                    $(".users tbody").append($tr);
                });

                $(".toggle-admin").click(function (event) {
                    event.preventDefault();
                    const $button = $(this);
                    const userId = $button.data("user-id");
                    const currentAdminStatus = $button.data("admin");

                    const data = {
                        action: "toggleAdminStatus",
                        userId: userId,
                        admin: !currentAdminStatus
                    };

                    ajax(data, function (response) {
                        const newAdminStatus = response["newAdminStatus"];
                        $button.data("admin", newAdminStatus);
                        $button.text(newAdminStatus ? "Disable" : "Enable");
                    })
                });
            })
        })
    </script>
</@c.page>
