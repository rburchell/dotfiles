if num_inbox >= 1
    # we ignore 'num', since well, we only care about inbox.
      notify_summary="sup found #{num_inbox} new message"
      if num_inbox > 1
          notify_summary << "s"
      end

      notify_body = ""
      from_and_subj.each { |f,s| notify_body += "#{f}:\n  #{s}\n\n" }
      system "/usr/bin/notify-send", "-i", "/usr/share/icons/Human/scalable/emblems/emblem-mail.svg", notify_summary, notify_body
end
