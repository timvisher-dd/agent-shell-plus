/* agent-shell-alert-mac.m -- Emacs dynamic module for macOS native notifications.
 *
 * Uses UNUserNotificationCenter to post desktop notifications from
 * GUI Emacs, which has a bundle identifier (e.g. org.gnu.Emacs).
 *
 * Build: cc -Wall -O2 -fPIC -shared -fobjc-arc \
 *          -I<emacs-include-dir> \
 *          -framework UserNotifications -framework Foundation \
 *          -o agent-shell-alert-mac.dylib agent-shell-alert-mac.m
 */

#include <string.h>
#include <stdlib.h>
#include <emacs-module.h>

#import <Foundation/Foundation.h>
#import <UserNotifications/UserNotifications.h>

/* Required by Emacs module API: GPL compatibility declaration. */
int plugin_is_GPL_compatible;

/* --- Helpers --- */

/* Extract a C string from an Emacs string value.  Caller must free(). */
static char *
extract_string(emacs_env *env, emacs_value val)
{
  ptrdiff_t len = 0;
  env->copy_string_contents(env, val, NULL, &len);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return NULL;

  char *buf = malloc(len);
  if (!buf)
    return NULL;

  env->copy_string_contents(env, val, buf, &len);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    free(buf);
    return NULL;
  }
  return buf;
}

/* Signal an Emacs error with a message string. */
static void
signal_error(emacs_env *env, const char *msg)
{
  emacs_value sym = env->intern(env, "error");
  emacs_value data = env->make_string(env, msg, strlen(msg));
  env->non_local_exit_signal(env, sym, data);
}

/* --- Notification operations --- */

/* (agent-shell-alert-mac-notify TITLE BODY)
 * Posts a macOS notification.  Returns t. */
static emacs_value
Fagent_shell_alert_mac_notify(emacs_env *env, ptrdiff_t nargs,
                              emacs_value *args, void *data)
{
  (void)nargs;
  (void)data;

  char *title_c = extract_string(env, args[0]);
  if (!title_c)
    return env->intern(env, "nil");
  char *body_c = extract_string(env, args[1]);
  if (!body_c) {
    free(title_c);
    return env->intern(env, "nil");
  }

  NSString *title = [NSString stringWithUTF8String:title_c];
  NSString *body = [NSString stringWithUTF8String:body_c];
  free(title_c);
  free(body_c);

  UNUserNotificationCenter *center =
      [UNUserNotificationCenter currentNotificationCenter];

  if (!center) {
    signal_error(env,
                 "agent-shell-alert-mac-notify: "
                 "UNUserNotificationCenter unavailable "
                 "(no bundle identifier?)");
    return env->intern(env, "nil");
  }

  UNMutableNotificationContent *content =
      [[UNMutableNotificationContent alloc] init];
  content.title = title;
  content.body = body;
  content.sound = [UNNotificationSound defaultSound];

  NSString *identifier =
      [NSString stringWithFormat:@"agent-shell-%f",
                                 [[NSDate date] timeIntervalSince1970]];
  UNNotificationRequest *request =
      [UNNotificationRequest requestWithIdentifier:identifier
                                           content:content
                                           trigger:nil];

  [center addNotificationRequest:request
           withCompletionHandler:^(NSError *error) {
             if (error) {
               NSLog(@"agent-shell-alert-mac: notification error: %@", error);
             }
           }];

  return env->intern(env, "t");
}

/* (agent-shell-alert-mac-request-authorization)
 * Requests notification authorization.  Returns t. */
static emacs_value
Fagent_shell_alert_mac_request_authorization(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
  (void)nargs;
  (void)args;
  (void)data;

  UNUserNotificationCenter *center =
      [UNUserNotificationCenter currentNotificationCenter];

  if (!center) {
    signal_error(env,
                 "agent-shell-alert-mac-request-authorization: "
                 "UNUserNotificationCenter unavailable "
                 "(no bundle identifier?)");
    return env->intern(env, "nil");
  }

  [center requestAuthorizationWithOptions:(UNAuthorizationOptionAlert |
                                           UNAuthorizationOptionSound)
                        completionHandler:^(BOOL granted, NSError *error) {
                          if (error) {
                            NSLog(@"agent-shell-alert-mac: authorization "
                                  @"error: %@",
                                  error);
                          } else if (!granted) {
                            NSLog(@"agent-shell-alert-mac: authorization "
                                  @"denied by user");
                          }
                        }];

  return env->intern(env, "t");
}

/* --- Module initialization --- */

static void
bind_function(emacs_env *env, const char *name, emacs_value func)
{
  emacs_value sym = env->intern(env, name);
  emacs_value args[] = {sym, func};
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int
emacs_module_init(struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment(runtime);

  if ((size_t)env->size < sizeof(*env))
    return 1;

  bind_function(
      env, "agent-shell-alert-mac-notify",
      env->make_function(
          env, 2, 2, Fagent_shell_alert_mac_notify,
          "Post a macOS native notification.\n\n"
          "(agent-shell-alert-mac-notify TITLE BODY)\n\n"
          "Uses UNUserNotificationCenter.  Returns t on success.",
          NULL));

  bind_function(
      env, "agent-shell-alert-mac-request-authorization",
      env->make_function(
          env, 0, 0, Fagent_shell_alert_mac_request_authorization,
          "Request macOS notification authorization.\n\n"
          "(agent-shell-alert-mac-request-authorization)\n\n"
          "Call once to prompt the user for notification permission.\n"
          "Returns t.",
          NULL));

  emacs_value feature = env->intern(env, "agent-shell-alert-mac");
  emacs_value provide_args[] = {feature};
  env->funcall(env, env->intern(env, "provide"), 1, provide_args);

  return 0;
}
