DoIncludeScript("nuts/util.nut", null);

const WALL_DISTANCE = 16.0;
const WALL_MAX_Z = 0.1;
const WALL_GRAVITY = 0.5;
const NORMAL_GRAVITY = 1.0;
const WALL_GRAVITY_MIN_VEL = 400;
const WALL_JUMP_NORMAL = 400;
const WALL_JUMP_UP = 200;
const WALL_JUMP_FORWARD = 200;
const WALL_JUMP_INTERVAL = 0.8;

function Think()
{
    local ply = null;
    while(ply = Entities.FindByClassname(ply, "player"))
    {
        if (IsValidAndAlive(ply))
        {
            local wallNormal = CheckPlayer(ply);
            if (wallNormal != null)
            {
                if (GetJumpCooldown(ply) <= 0)
                {
                    local buttons = GetButtons(ply);
                    if (buttons & Constants.FButtons.IN_JUMP)
                    {
                        local vel = ply.GetAbsVelocity();
                        if (vel.z < WALL_JUMP_UP)
                        {
                            vel.z = WALL_JUMP_UP;
                        }
                        vel += wallNormal * WALL_JUMP_NORMAL;

                        local wallForward = wallNormal.Cross(Vector(0, 0, 1));
                        if (wallForward.Dot(vel) < 0)
                        {
                            wallForward *= -1;
                        }

                        vel += wallForward * WALL_JUMP_FORWARD;

                        ply.SetAbsVelocity(vel);
                        ::nuts_wallrun_players[ply.entindex()].last_jump = Time();
                        NetProps.SetPropFloat(ply, "m_flGravity", NORMAL_GRAVITY);
                        continue;
                    }
                }

                if (ply.GetAbsVelocity().Length2D() >= WALL_GRAVITY_MIN_VEL)
                {
                    NetProps.SetPropFloat(ply, "m_flGravity", WALL_GRAVITY);
                }
                else
                {
                    NetProps.SetPropFloat(ply, "m_flGravity", NORMAL_GRAVITY);
                }
            }
            else
            {
                NetProps.SetPropFloat(ply, "m_flGravity", NORMAL_GRAVITY);
            }

            PrintJumpCharge(ply);
        }
    }
    // think every frame
    return 0;
}

function GetJumpCooldown(ply)
{
    local cooldown = ::nuts_wallrun_players[ply.entindex()].last_jump + WALL_JUMP_INTERVAL - Time();
    if (cooldown < 0)
    {
        cooldown = 0;
    }
    return cooldown;
}

function PrintJumpCharge(ply)
{
    local cooldown = GetJumpCooldown(ply);
    local chargeBars = 0;

    // avoid flickering on the floor
    if (cooldown < WALL_JUMP_INTERVAL - FrameTime())
    {
        chargeBars = 10.0 * (1.0 - GetJumpCooldown(ply) / WALL_JUMP_INTERVAL);
    }

    local chargeText = "|";
    for (local i = 0; i < 10; i++)
    {
        if (i < chargeBars)
        {
            chargeText += "#";
        }
        else
        {
            chargeText += "--";
        }
    }
    chargeText += "|";
    ClientPrint(ply, Constants.EHudNotify.HUD_PRINTCENTER, chargeText);
}

function CheckPlayer(ply)
{
    local ground = GetGroundEntity(ply);
    if (ground != null)
    {
        ::nuts_wallrun_players[ply.entindex()].last_jump = Time();
        return null;
    }

    local wish = GetWishDir(ply);
    if (!wish.valid)
    {
        return null;
    }

    local trace = TraceBox(
        ply.GetOrigin(),
        ply.GetOrigin() + wish.dir * WALL_DISTANCE,
        ply.GetPlayerMins(),
        ply.GetPlayerMaxs(),
        MASK_PLAYER_SOLID_BRUSH_ONLY,
        ply
    );
    if (trace.fraction >= 1.0 - Constants.Math.Epsilon)
    {
        return null;
    }

    local normal = trace.plane_normal;
    if (fabs(normal.z) > WALL_MAX_Z)
    {
        return null;
    }

    normal.z = 0;
    normal.Norm();
    local hVel = ply.GetAbsVelocity();
    hVel.z = 0;
    local dot = hVel.Dot(normal);
    if (dot > 0)
    {
        return null;
    }

    return normal;
}

if (!("nuts_wallrun" in getroottable()))
{
    Log("nuts_wallrun init");
    ::nuts_wallrun <- true;
    ::nuts_wallrun_players <- {};
    for (local i = 0; i < Constants.Server.MAX_PLAYERS; i+=1)
    {
        ::nuts_wallrun_players[i] <- { last_jump = 0 };
    }

    if (!("HOOKED_EVENTS" in getroottable()))
    {
        Log("nuts_wallrun hook events");
        __CollectGameEventCallbacks(this);
        ::HOOKED_EVENTS <- true;
    }

    local thinker = SpawnEntityFromTable("info_target", { targetname = "nuts_wallrun_thinker" } );
    if(thinker.ValidateScriptScope())
    {
        Log("nuts_wallrun thinker valid");
        thinker.GetScriptScope()["Think"] <- Think;
        AddThinkToEnt(thinker, "Think");
    }
}
else
{
    Log("nuts_wallrun already init");
}

